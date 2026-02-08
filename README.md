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
      4. Image support.

## Use with Emacs

1. Load/require vscode-cp-proxy.el
2. M-x vscode-cp-proxy-set-gptel-backend 
   1. You can generate a secure auth token and set it in VS Code by
      executing the command setToken provided by this extension. The
      token can then be stored in the authinfo gpg file and used in
      Emacs -
      ```
        (setq vscode-cp-proxy-token (lambda () (cadr (auth-source-user-and-password "vscode-cp-proxy"))))
      ```
   2. By default, the extension will generate a new token on every
      launch which can be obtained from output window by selecting
      vscode-cp-proxy in VS Code.
   3. Choose from available models

## demo - usage with emacs
[UsageWithEmacs](./demo/vscode-cp-proxy.gif "Usage with Emacs")
