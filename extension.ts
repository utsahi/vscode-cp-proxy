import * as vscode from 'vscode';
import { SecretStorage } from 'vscode';
import * as http from 'http';

import { OpenAiRequestPayload, OpenAiChatMessage, OpenAiMessageContent } from './payload-schema';
import { ITracingOutputChannel, TracingOutputChannelImpl } from './tracer';

let server: http.Server | undefined;
let tokenInfoInterval: NodeJS.Timeout | undefined;

export async function activate(context: vscode.ExtensionContext) {
    const secrets: SecretStorage = context.secrets;
    const config = vscode.workspace.getConfiguration('vscodeCpProxy');
    const PORT = config.get<number>('port', 5555);
    const HOST = config.get<string>('host', '127.0.0.1');
    const verbosity = config.get<string>('verbosity', 'info');
    const secretTokenKey = "authToken";

    let tokenFromSecretStorage = false;
    const defaultTokenFactory = () => new Date().toISOString();
    let token = '';

    let traceEndpointInfo = () =>
        tracer.info(`Listening on http://${HOST}:${PORT}. ${tokenFromSecretStorage ? "Using token from the secret store." : "Use this auto generated (weak) token without quotes for auth: " + token}`);
    
    const output = vscode.window.createOutputChannel('vscode-cp-proxy');
    const tracer: ITracingOutputChannel = new TracingOutputChannelImpl(output, verbosity);

    let setTokenCommand = vscode.commands.registerCommand('vscodeCpProxy.setToken', async () => {
        const inputToken = await vscode.window.showInputBox({
            placeHolder: 'Enter the token for use during authentication.',
            prompt: 'Enter the token for use during authentication.',
            ignoreFocusOut: true
        });
        if (inputToken) {
            await secrets.store(secretTokenKey, inputToken);
            token = inputToken;
            tokenFromSecretStorage = true;
            vscode.window.showInformationMessage('Token set successfully.');
        } 
        traceEndpointInfo();
    });
    context.subscriptions.push(setTokenCommand);

    let clearTokenCommand = vscode.commands.registerCommand('vscodeCpProxy.clearToken', async () => {
        await secrets.delete(secretTokenKey);
        token = defaultTokenFactory();
        tokenFromSecretStorage = false;
        vscode.window.showInformationMessage('Token cleared successfully.');
        
        traceEndpointInfo();
    });
    context.subscriptions.push(clearTokenCommand);
    
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
            const chunks: Buffer[] = [];
            req.on('data', (chunk) => {
                chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(String(chunk)));
            });
            req.on('end', async () => {
                const decoded = decodeRequestBody(req, chunks, tracer);
                if (!decoded.ok) {
                    res.writeHead(decoded.status ?? 500, { 'Content-Type': 'text/plain; charset=utf-8' });
                    res.end(decoded.message || '', 'utf8');
                    return;
                }
                const body = decoded.body!;
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
                    name: m.name,
                    vendor: m.vendor,
                    family: m.family,
                    version: m.version,
                    maxTokens: typeof m.maxInputTokens === 'number' ? m.maxInputTokens : undefined,
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

        let secretToken = await secrets.get(secretTokenKey);
        tokenFromSecretStorage = secretToken ? true : false;
        token = config.get<string>('token')?.trim() || secretToken || defaultTokenFactory();

        server.listen(PORT, HOST, traceEndpointInfo);

        tokenInfoInterval = setInterval(traceEndpointInfo, 5 * 60 * 1000);

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

async function fetchAllModels(tracer: ITracingOutputChannel): Promise<vscode.LanguageModelChat[]> {
    
    tracer.info("Fetching models...");
    const models = await vscode.lm.selectChatModels();
    models.forEach((m) => tracer.info("Model.id: " + m.id));
    return models;
}

function base64ToUint8Array(base64: string): Uint8Array {
    const buf = Buffer.from(base64, 'base64');
    return new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength);
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

function parseContentType(contentTypeHeader: string | undefined): { mimeType: string; charset?: string } {
    const header = String(contentTypeHeader || '').trim();
    if (header === '') return { mimeType: 'application/octet-stream' };
    const parts = header.split(';').map(p => p.trim());
    const mimeType = (parts[0] || 'application/octet-stream').toLowerCase();
    let charset: string | undefined;
    for (let i = 1; i < parts.length; i++) {
        const m = /^charset=(.+)$/i.exec(parts[i]);
        if (m) {
            charset = m[1].trim().toLowerCase();
            break;
        }
    }
    return { mimeType, charset };
}

function isTextLike(mimeType: string): boolean {
    return mimeType.startsWith('text/')
        || mimeType === 'application/json'
        || mimeType.endsWith('+json')
        || mimeType === 'application/x-www-form-urlencoded';
}

function decodeRequestBody(req: http.IncomingMessage, chunks: Buffer[], tracer: ITracingOutputChannel): { ok: boolean; body?: string; status?: number; message?: string } {
    const bodyBuffer = Buffer.concat(chunks);

    const { mimeType, charset } = parseContentType(req.headers['content-type']);
    const effectiveCharset = charset ?? 'utf-8';

    if (!isTextLike(mimeType)) {
        tracer.warn(`Received unsupported content-type '${mimeType}'. Only JSON/text content is accepted.`);
        return { ok: false, status: 415, message: 'Unsupported Media Type' };
    }

    if (effectiveCharset !== 'utf-8' && effectiveCharset !== 'utf8') {
        tracer.warn(`Unsupported charset '${effectiveCharset}'.`);
        return { ok: false, status: 415, message: `Unsupported charset: ${effectiveCharset}` };
    }

    const body = bodyBuffer.toString('utf8');
    tracer.info(`Request length ${Buffer.byteLength(body, 'utf8')} bytes.`);
    return { ok: true, body };
}

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
            let contents = msg.tool_calls.map((t) => {
                if (t.type !== undefined && t.type !== "function") {
                    throw new Error(
                        `Unsupported tool_call type '${t.type}' for tool_call id '${t.id}'. ` +
                        `Only 'function' tool calls are supported. ` +
                        `Update your client to send tool_calls with type='function'.`
                    );
                }
                const name = t.function?.name ?? t.name;
                if (!name) {
                    throw new Error(
                        `Missing tool name in assistant tool_call (id '${t.id}'). ` +
                        `Expected either 'function.name' (standard OpenAI shape) or top-level 'name'. ` +
                        `Check your client's tool_call serialization.`
                    );
                }
                const rawArgs = t.function?.arguments ?? t.args;
                const parsedArgs = (rawArgs === undefined || rawArgs === null || rawArgs === "")
                    ? {}
                    : (typeof rawArgs === "string" ? JSON.parse(rawArgs) : rawArgs);
                return new vscode.LanguageModelToolCallPart(
                    t.id,
                    name,
                    parsedArgs
                );
            });
           
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

    const matchedModels = await vscode.lm.selectChatModels({ id: payload.model });
    const matchedModel = matchedModels && matchedModels.length > 0 ? matchedModels[0] : undefined;
    if (!matchedModel) {
        const allModels = await fetchAllModels(tracer);
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
