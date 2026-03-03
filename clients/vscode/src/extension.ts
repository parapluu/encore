'use strict';

import * as path from 'path';

import { workspace, ExtensionContext, window } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient';
import { Socket, createServer } from 'net';
const cp = require("child_process");

export function activate(context: ExtensionContext) {
	console.log('encore LSP activated');
	
	const serverOptions: ServerOptions = function () {
		return new Promise((resolve, reject) => {
			let childProcess

			const server = createServer(socket => {
				server.close()
				resolve({ reader: socket, writer: socket })
			})

			server.listen(0, '127.0.0.1', () => {
				let port = server.address().port
				childProcess = cp.spawn("encorec", ["--server", "tcp-client", "--host", "localhost", "--port", `${port}`])

				childProcess.stderr.on('data', (chunk: Buffer) => {
					console.error(chunk + '');
				});
				childProcess.stdout.on('data', (chunk: Buffer) => {
					console.log(chunk + '');
				});

				return childProcess
			})
		})
	}

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{scheme: 'file', language: 'encore'}],
		synchronize: {
			configurationSection: 'encorelangsvr',
			fileEvents: workspace.createFileSystemWatcher('**/*.enc')
		}
	}

	let languageClient = new LanguageClient('encorelangsvr', 'Language Server Encore', serverOptions, clientOptions);
	let disposable = languageClient.start();

	context.subscriptions.push(disposable);
}

export let config = workspace.getConfiguration("encore");
