import * as http from 'http';
import * as https from 'https';
import * as tls from 'tls';
import { ITracingOutputChannel } from './tracer';
export const DEEPINFRA_FLEX_PATH = '/deepinfra/flex/openai/v1/chat/completions';

const UPSTREAM_HOST = 'api.deepinfra.com';
const UPSTREAM_PATH = '/v1/openai/chat/completions';
const UPSTREAM_IDLE_TIMEOUT_MS = 300_000;
const EXTRA_OPTIONS: Record<string, unknown> = {
    service_tier: 'flex',
    fail_fast: true,
};

const HOP_BY_HOP_HEADERS = new Set([
    'connection',
    'keep-alive',
    'proxy-authenticate',
    'proxy-authorization',
    'te',
    'trailer',
    'transfer-encoding',
    'upgrade',
]);

function readBody(req: http.IncomingMessage, tracer: ITracingOutputChannel): Promise<Buffer> {
    return new Promise((resolve, reject) => {
        const chunks: Buffer[] = [];
        req.on('data', (chunk) => {
            chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(String(chunk)));
        });
        req.on('end', () => resolve(Buffer.concat(chunks)));
        req.on('error', (err) => reject(err));
    });
}

function buildForwardHeaders(bodyLength: number, authorization: string): http.OutgoingHttpHeaders {
    return {
        'content-type': 'application/json',
        'accept': 'application/json',
        'authorization': authorization,
        'content-length': bodyLength,
    };
}

function redactHeaders(headers: http.IncomingHttpHeaders | http.OutgoingHttpHeaders): http.OutgoingHttpHeaders {
    const redacted: http.OutgoingHttpHeaders = {};
    for (const [name, value] of Object.entries(headers)) {
        redacted[name] = name.toLowerCase() === 'authorization' ? 'Bearer <redacted>' : value;
    }
    return redacted;
}

function injectExtraOptions(
    body: Buffer,
    extraOptions: Record<string, unknown>,
    tracer: ITracingOutputChannel
): Buffer {
    if (body.length === 0) {
        tracer.warn('Request body is empty. Extra options were not injected.');
        return body;
    }
    let payload: unknown;
    try {
        payload = JSON.parse(body.toString('utf8'));
    } catch (err) {
        tracer.error('Failed to parse request body as JSON: ' + (err instanceof Error ? err.message : String(err)));
        return body;
    }
    if (payload === null || typeof payload !== 'object' || Array.isArray(payload)) {
        tracer.warn('Request body is not a JSON object. Extra options were not injected.');
        return body;
    }
    const record = payload as Record<string, unknown>;
    for (const [key, value] of Object.entries(extraOptions)) {
        record[key] = value;
    }
    tracer.debug(() => 'Forwarding payload: ' + JSON.stringify(record));
    return Buffer.from(JSON.stringify(record), 'utf8');
}

export function handleDeepInfraFlexRequest(
    req: http.IncomingMessage,
    res: http.ServerResponse,
    tracer: ITracingOutputChannel
): void {
    tracer.info(`DeepInfra flex request received: ${req.method ?? 'UNKNOWN'} ${req.url ?? 'NO URL'}`);
    tracer.debug(() => 'Incoming headers: ' + JSON.stringify(redactHeaders(req.headers)));

    if (req.method !== 'POST') {
        tracer.warn(`Method ${req.method ?? 'UNKNOWN'} is not allowed on ${DEEPINFRA_FLEX_PATH}.`);
        res.writeHead(405, { 'Content-Type': 'text/plain; charset=utf-8', 'Allow': 'POST' });
        res.end('Method Not Allowed', 'utf8');
        return;
    }

    const authorization = req.headers['authorization'];
    if (!authorization) {
        tracer.error('The incoming request has no Authorization header. The flex endpoint forwards it as is.');
        res.writeHead(400, { 'Content-Type': 'text/plain; charset=utf-8' });
        res.end('The incoming request has no Authorization header.', 'utf8');
        return;
    }

    readBody(req, tracer).then((body) => {
        tracer.info(`Incoming body length ${body.length} bytes.`);
        const modifiedBody = injectExtraOptions(body, EXTRA_OPTIONS, tracer);
        tracer.info(`Outgoing body length ${modifiedBody.length} bytes. Injected options: ${Object.keys(EXTRA_OPTIONS).join(', ')}`);

        const headers = buildForwardHeaders(modifiedBody.length, authorization);
        const requestOptions = {
            hostname: UPSTREAM_HOST,
            port: 443,
            path: UPSTREAM_PATH,
            method: 'POST',
            headers,
        };
        tracer.debug(() => 'Outgoing request: POST https://' + UPSTREAM_HOST + ':' + requestOptions.port + UPSTREAM_PATH);
        tracer.debug(() => 'Outgoing headers: ' + JSON.stringify(redactHeaders(headers)));
        tracer.debug(() => 'Outgoing body: ' + modifiedBody.toString('utf8'));

        const upstream = https.request(
            requestOptions,
            (upstreamRes) => {
                tracer.info(`Upstream responded with status ${upstreamRes.statusCode ?? 0}.`);
                tracer.debug(() => 'Upstream response headers: ' + JSON.stringify(upstreamRes.headers));
                const responseHeaders: http.OutgoingHttpHeaders = {};
                for (const [name, value] of Object.entries(upstreamRes.headers)) {
                    if (value === undefined || HOP_BY_HOP_HEADERS.has(name.toLowerCase())) {
                        continue;
                    }
                    responseHeaders[name] = value;
                }
                res.writeHead(upstreamRes.statusCode ?? 502, responseHeaders);
                upstreamRes.pipe(res);
                upstreamRes.on('end', () => {
                    tracer.info('Upstream response fully relayed.');
                });
            }
        );

        upstream.on('socket', (socket) => {
            tracer.debug(() => 'Upstream socket assigned. Connecting to ' + UPSTREAM_HOST + ':443.');
            if (!(socket instanceof tls.TLSSocket)) {
                return;
            }
            socket.on('secureConnect', () => {
                tracer.debug(() => 'Upstream TLS handshake complete. Authorized: ' + socket.authorized
                    + (socket.authorizationError ? ', error: ' + String(socket.authorizationError) : ''));
            });
        });

        upstream.setTimeout(UPSTREAM_IDLE_TIMEOUT_MS, () => {
            tracer.error(`Upstream was idle for ${UPSTREAM_IDLE_TIMEOUT_MS} ms. Aborting the request.`);
            upstream.destroy(new Error(`Upstream idle timeout after ${UPSTREAM_IDLE_TIMEOUT_MS} ms`));
        });

        upstream.on('error', (err) => {
            tracer.error('Upstream request failed: ' + err.message);
            if (!res.headersSent) {
                res.writeHead(502, { 'Content-Type': 'text/plain; charset=utf-8' });
            }
            res.end('Upstream request failed: ' + err.message, 'utf8');
        });

        res.on('close', () => {
            if (!res.writableEnded) {
                tracer.warn('Client closed the connection before the response completed.');
                upstream.destroy();
            }
        });

        upstream.end(modifiedBody);
    }).catch((err) => {
        tracer.error('Failed to read request body: ' + (err instanceof Error ? err.message : String(err)));
        if (!res.headersSent) {
            res.writeHead(400, { 'Content-Type': 'text/plain; charset=utf-8' });
        }
        res.end('Failed to read request body', 'utf8');
    });
}
