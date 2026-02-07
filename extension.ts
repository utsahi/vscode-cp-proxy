import * as vscode from 'vscode';
import * as http from 'http';
import { OpenAiRequestPayload, OpenAiChatMessage, OpenAiMessageContent } from './payload-schema';
import { ITracingOutputChannel, TracingOutputChannelImpl } from './tracer';

let server: http.Server | undefined;
let tokenInfoInterval: NodeJS.Timeout | undefined;

export function activate(context: vscode.ExtensionContext) {

    const config = vscode.workspace.getConfiguration('vscodeCpProxy');
    const PORT = config.get<number>('port', 5555);
    const HOST = config.get<string>('host', '127.0.0.1');
    const verbosity = config.get<string>('verbosity', 'info');

    const token = config.get<string>('token')?.trim() || new Date().toISOString();
    const output = vscode.window.createOutputChannel('vscode-cp-proxy');
    const tracer: ITracingOutputChannel = new TracingOutputChannelImpl(output, verbosity);

    server = http.createServer((req, res) => {
        res.setHeader('Content-Type', 'text/plain; charset=utf-8');
       
        const authHeader = req.headers['authorization'];
        const expectedToken = `Bearer ${token}`;

        if (!authHeader || authHeader !== expectedToken) {
            tracer.error('Authentication failed');
            res.statusCode = 401;
            res.setHeader('WWW-Authenticate', 'Bearer');
            res.end('Unauthorized', 'utf8');
            return;
        }
       
        tracer.info(req.url ?? "NO URL!");
        if (req.url === '/openai/v1/chat/completions') {
            let body = '';
            req.on('data', (chunk) => {
                body += chunk.toString();
            });
            req.on('end', async () => {
                tracer.info(`Request length ${Buffer.byteLength(body, 'utf8')} bytes.`);
                let payload: OpenAiRequestPayload;
                let wroteHead = false;
                let id: string = "request-id";
                try {
                    payload = JSON.parse(body) as OpenAiRequestPayload;

                    res.writeHead(200, {
                        'Content-Type': payload.stream ? 'text/event-stream; charset=utf-8' : 'text/plain; charset=utf-8',
                        'Cache-Control': 'no-cache',
                        'Connection': 'keep-alive'
                    });
                    wroteHead = true;

                    let stringChunkCount = 0;
                    let totalLength = 0;
                    for await (const chunk of openAiChat(id, payload, tracer)) {
                        stringChunkCount++;
                        const chunkBytes = Buffer.byteLength(chunk, 'utf8');
                        totalLength += chunkBytes;
                        res.write(chunk, 'utf8');
                    }
                    tracer.info(`Stream: ${payload.stream}, Response ${stringChunkCount} chunks with total length ${totalLength} bytes.`);

                    res.end();
                } catch (e) {
                    let errStr = JSON.stringify(e, Object.getOwnPropertyNames(e));
                    tracer.error(errStr);
                    if (!wroteHead) {
                        res.writeHead(500, { 'Content-Type': 'text/plain; charset=utf-8' });
                    }
                    const errorChunk = 'data: ' + toOpenAICompletionChunk(id, errStr, true) + '\n\n';
                    res.end(errorChunk, 'utf8');

                    tracer.error("Stack trace: " + ((e as Error)?.stack ?? "No stack trace available."));
                    return;
                }
            });
        } else if (req.url === '/openai/v1/models') {
            res.setHeader('Content-Type', 'application/json; charset=utf-8');
            res.writeHead(200);
            fetchAllModels(tracer).then(models => {
                const now = Math.floor(Date.now() / 1000);
                const data = models.map((m, idx) => ({
                    id: m.id,
                    object: "model",
                    created: now,
                    owned_by: "organization-owner"
                }));
                const response = {
                    object: "list",
                    data: data
                };
                res.end(JSON.stringify(response), 'utf8');
            }).catch(err => {
                tracer.error('Failed to enumerate models: ' + (err instanceof Error ? err.message : String(err)));
                res.writeHead(500, { 'Content-Type': 'application/json; charset=utf-8' });
                res.end(JSON.stringify({ error: 'Failed to enumerate models' }), 'utf8');
            });
        } else {
            tracer.error('Url not found: ' + req.url);
            res.writeHead(404, { 'Content-Type': 'text/plain; charset=utf-8' });
            res.end('Url not found: ' + req.url, 'utf8');
            return;
        }
    });

    try {
        const traceMessage = `Listening on http://${HOST}:${PORT}, use this (weak) token without quotes for auth: '${token}'`;

        server.listen(PORT, HOST, () => {
            tracer.info(traceMessage);
        });

        tokenInfoInterval = setInterval(() => {
            tracer.info(traceMessage);
        }, 5 * 60 * 1000);

    } catch (err) {
        tracer.error('Failed to start server: ' + (err instanceof Error ? err.message : String(err)));
    }

    context.subscriptions.push({
        dispose: () => {
            deactivate();
        },
    });
}

function toOpenAICompletionChunk(id: string, content: string, finish: boolean) {
    return JSON.stringify({
            id: id,
            object: "chat.completion.chunk",
            finish_reason: finish ? "stop" : null,
            choices: [
                {
                    delta: {
                        content: content,
                    },
                },
            ],
    });
}

function toolCallId(toolCall: vscode.LanguageModelToolCallPart, tracer: ITracingOutputChannel) {
    let callId : string;
    if (toolCall.callId) {
        callId = toolCall.callId;
    } else {
        callId = `generated-${new Date().toISOString()}`;
        tracer.warn(`Generated a tool-call id: ${callId}`);
    }
    return callId;
}

function* toOpenAIToolCallChunks(tracer: ITracingOutputChannel, toolCall: vscode.LanguageModelToolCallPart) : Iterable<string> {
    const callId = toolCallId(toolCall, tracer);
    yield JSON.stringify({
        choices: [
            {
                delta: {
                    tool_calls: [
                        {
                            function: {
                                arguments: JSON.stringify(toolCall.input),
                                name: toolCall.name,
                            },
                            id: callId,
                            name: toolCall.name,
                            args: JSON.stringify(toolCall.input),
                        }
                    ]
                },
                finish_reason: "tool_call",
            }
        ],
        id: "chatcmpl-" + toolCall.callId,
        object: "chat.completion.chunk",
    });
}

export function deactivate() {
    server?.close();
    if (tokenInfoInterval) {
        clearInterval(tokenInfoInterval);
        tokenInfoInterval = undefined;
    }
}

let cachedModels: vscode.LanguageModelChat[] = [];
let cacheLastUpdated: number = 0;
async function fetchAllModels(tracer: ITracingOutputChannel): Promise<vscode.LanguageModelChat[]> {
    const now = Date.now();

    if (now - cacheLastUpdated > 30 * 60 * 1000) {
        tracer.info("Updating models cache...");
        cachedModels = await vscode.lm.selectChatModels();
        cachedModels.forEach((m) => tracer.info("Model.id: " + m.id));
        cacheLastUpdated = now;
    }

    tracer.info("Returning from models cache...");
    return cachedModels;
}

function base64ToUint8Array(base64: string): Uint8Array {
    const binaryString = atob(base64);
    const bytes = new Uint8Array(binaryString.length);
    for (let i = 0; i < binaryString.length; i++) {
        bytes[i] = binaryString.charCodeAt(i);
    }
    return bytes;
}

function unpackDataUrl (dataUrl: string): { mimeType: string; base64Content: string } {
    const match = dataUrl.match(/^data:(.*?);base64,(.+)$/);
    if (!match) {
        throw new Error("Invalid data URL format.");
    }
    return {
        mimeType: match[1],
        base64Content: match[2],
    };
};

function composeChatMessage(msg: OpenAiChatMessage) : vscode.LanguageModelChatMessage{
    let toolCallResult = msg.role.toLocaleLowerCase() === "tool";

    let role: vscode.LanguageModelChatMessageRole =
        (toolCallResult || msg.role.toLowerCase() === 'user')
            ? vscode.LanguageModelChatMessageRole.User
            : vscode.LanguageModelChatMessageRole.Assistant;

    if (role === vscode.LanguageModelChatMessageRole.User) {
        if (toolCallResult) {
            if (typeof msg.content === 'string') {
                let content = [new vscode.LanguageModelToolResultPart(
                    msg.tool_call_id,
                    [new vscode.LanguageModelTextPart(msg.content)]
                )];
                return {
                    role: role,
                    content: content,
                    name: undefined,
                };
            }
            else {
                throw Error("Tool call result type is unknown.");
            }
        } else {
            let content;
            if (typeof msg.content === 'string') {
                content = [new vscode.LanguageModelTextPart(msg.content)];
            }
            else {
                content = msg.content.map((c) =>
                    {
                        if (c.type === "text") {
                            return new vscode.LanguageModelTextPart(c.text);
                        } else if (c.type === "image_url") {
                            let unpackgedImage = unpackDataUrl(c.image_url.url);
                            let arr = base64ToUint8Array(unpackgedImage.base64Content);
                            return new vscode.LanguageModelDataPart(arr, unpackgedImage.mimeType);
                        }
                        else {
                            throw Error("Unknown content element " + c.type);
                        }
                    }
                );
            }

            return {
                role: role,
                content: content,
                name: undefined,
            };
        }
    }
    else if (role == vscode.LanguageModelChatMessageRole.Assistant) {
        if (!msg.content) {
            let contents = msg.tool_calls.map((t) =>
                new vscode.LanguageModelToolCallPart(
                    t.id,
                    t.name,
                    JSON.parse(t.args)
                )
            );
           
            return {
                role: role,
                content: contents,
                name: undefined,
            };
        }
        else {
            if (typeof msg.content === 'string') {
                let textPart = [new vscode.LanguageModelTextPart(msg.content)];

                return {
                    role: role,
                    content: textPart,
                    name: undefined,
                };
            } else {
                throw Error("Unknown assistant role content ");
            }
        }
    }
    else {
        throw Error(`Unknown role ${role}`);
    }
}

async function* openAiChat(reqId: string, payload: OpenAiRequestPayload, tracer: ITracingOutputChannel): AsyncIterable<string> {
    tracer.debug(() => "Payload: " + JSON.stringify(payload));
    const messages: vscode.LanguageModelChatMessage[] = payload.messages.map((msg) => composeChatMessage(msg));

    tracer.debug(() => JSON.stringify(messages));

    const allModels = await fetchAllModels(tracer);
    let matchedModel = allModels.find((m) => m.id === payload.model);
    if (!matchedModel) {
        const availableModelIds = allModels.map((m) => m.id);
        throw new Error(`Model '${payload.model}' not found. Available models: ${availableModelIds.join(', ')}`);
    }

    let requestOptions: vscode.LanguageModelChatRequestOptions = {
        tools: payload.tools?.
   filter((t) => t.type === "function")
            .map((f): vscode.LanguageModelChatTool => ({
                name: f.function.name,
                description: f.function.description,
                inputSchema: f.function.parameters,
            })),
    };

    const response = await matchedModel.sendRequest(messages, requestOptions);
    tracer.info("Request sent.");

    if (payload.stream) {
        for await (const chunk of response.stream) {
            if (chunk instanceof vscode.LanguageModelTextPart){
                yield 'data: ' + toOpenAICompletionChunk(reqId, chunk.value, false) + '\n\n';
            }
            else if (chunk instanceof vscode.LanguageModelToolCallPart){
                tracer.debug(() => "constructing tool call chunks. " + JSON.stringify(chunk));
                for (const tcChunk of toOpenAIToolCallChunks(tracer, chunk)){
                    yield 'data: ' + tcChunk + '\n\n';
                }
            }
            else{
              if (chunk instanceof vscode.LanguageModelDataPart){
                tracer.error("LanguageModelDataPart was received and ignored.");
              }
              else {
                tracer.error("Unknown object was received and ignored.");
                tracer.debug(() => "Unknown object was received and ignored: " + JSON.stringify(chunk));
              }
            }
        }
        yield 'data: [DONE]\n\n\n';
    } else {
        let text = "";
        for await (const chunk of response.stream) {
            if (chunk instanceof vscode.LanguageModelTextPart){
                text += chunk.value;
            }
            else if (chunk instanceof vscode.LanguageModelToolCallPart){
                const callId = toolCallId(chunk, tracer);
                yield JSON.stringify(
                    {
                        "object": "chat.completion",
                        "choices": [
                            {
                                "index": 0,
                                "message": {
                                    "role": "assistant",
                                    "content": null,
                                    tool_calls: [
                                        {
                                            function: {
                                                arguments: JSON.stringify(chunk.input),
                                                name: chunk.name,
                                            },
                                            id: callId,
                                            name: chunk.name,
                                            args: JSON.stringify(chunk.input),
                                        }
                                    ]
                                },
                                "finish_reason": "tool_calls"
                            }
                        ],                    
                    });
            }
            else{
                throw new Error(`Unknown type ${typeof(chunk)}`);
            }
        }

        if (text !== "") {
            yield JSON.stringify(
                {
                    "object": "chat.completion",
                    "choices": [
                        {
                            "index": 0,
                            "message": {
                                "role": "assistant",
                                "content": text,                  
                            },
                            "finish_reason": "stop"
                        }
                    ],
                });
        }
    }
}
