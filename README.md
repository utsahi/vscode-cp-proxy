# vscode-cp-proxy

A proxy server/adapter layer that relays requests to VS Code APIs.

Listens at localhost:5555 by default.

Open output/vscode-cp-proxy for authentication token for client
authentication with the proxy.

## Currently supported endpoints:

1. OpenAI
   1. /openai/v1/models
      1. Lists the models supported 
   2. /openai/v1/chat/completions
      1. Translates the incoming requests in OpenAI format into VS
         Code chat API invocations.
      2. Streams responses by default. Non-streaming behavior not
         tested.
      3. Supports tool calling.

## Use with Emacs

1. Load/require vscode-cp-proxy.el
2. M-x vscode-cp-proxy-set-gptel-backend 
   1. Enter the token obtained from output/vscode-cp-proxy from VS Code.
   2. Choose from available models

## Known issue(s)
1. If a model, e.g. Sonnet4 emits partial output and quits, try
   upgrading VS Code version.
