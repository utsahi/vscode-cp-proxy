import * as vscode from 'vscode';

export interface ITracingOutputChannel {
    trace(message: () => string): void;
    debug(message: () => string): void;
    info(message: string): void;
    warn(message: string): void;
    error(message: string): void;
}

export class TracingOutputChannelImpl implements ITracingOutputChannel {
    private output: vscode.OutputChannel;
    private currentLevel: number;
    private levels: Record<string, number> = {
        trace: 0,
        debug: 1,
        info: 2,
        warn: 3,
        error: 4,
    };

    constructor(outputChannel: vscode.OutputChannel, verbosity: string) {
        this.output = outputChannel;
        this.currentLevel = this.levels[verbosity] ?? this.levels.info;
    }

    private log(level: string, message: string | (() => string)) {
        if (this.currentLevel <= this.levels[level]) {
            const timestamp = new Date().toISOString();
            const msg = typeof message === 'function' ? message() : message;
            this.output.appendLine(`[${timestamp}] [${level.toUpperCase()}] ${msg}`);
        }
    }

    trace(message: () => string) {
        this.log('trace', message);
    }

    debug(message: () => string) {
        this.log('debug', message);
    }

    info(message: string) {
        this.log('info', message);
    }

    warn(message: string) {
        this.log('warn', message);
    }

    error(message: string) {
        this.log('error', message);
    }
}
