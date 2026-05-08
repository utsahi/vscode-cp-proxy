export interface OpenAiToolCall {
    id: string,
    type?: string,
    name?: string,
    args?: string,
    function?: {
        name: string,
        arguments: string,
    },
}

export interface OpenAiMessageContent
{
    type: 'text',
    text: string,
    image_url: {
        url: string
    }
}

export interface OpenAiChatMessage {
    role: 'user' | 'assistant' | 'tool' | 'system';
    content: string | [OpenAiMessageContent];
    tool_call_id: string;
    tool_calls: OpenAiToolCall[];
}

export interface OpenAiChatTool
{
  type: string,
  function: {
    name: string,
    description: string,
    parameters: {
      type: string,
      properties?: object,
      required?: string[],
    }
  }
}

export interface OpenAiRequestPayload {
    model: string;
    messages: OpenAiChatMessage[];
    temperature?: number;
    max_tokens?: number;
    stream: boolean;
    tools?: OpenAiChatTool[];
}
