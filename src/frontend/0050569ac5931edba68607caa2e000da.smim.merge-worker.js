var Module = {
	locateFile: s => `https://unpkg.com/wasm-git@0.0.4/${s}`
}

importScripts('https://unpkg.com/wasm-git@0.0.4/lg2.js');

Module.onRuntimeInitialized = () => {
	const lg = Module;
	FS.mkdir('/working');
	FS.mount(MEMFS, { }, '/working');

	const printError = err;
	let processStep;
	let terminate;
	err = obj => {
		printError(obj);
		terminate = true;
		postMessage({state: 'terminated', error: obj, processStep});
	};
	onmessage = (event) => {	
		FS.writeFile('/home/web_user/.gitconfig', '[user]\n' +
			`name = ${event.data.name}\n` +
			`email = ${event.data.email}`);

		processStep = 'clone'
		lg.callMain([processStep, event.data.url, event.data.repo]);
		if (terminate)
			return;
		FS.chdir(event.data.repo);

		processStep = 'checkout';
		lg.callMain([processStep, event.data.sourceBranch]);
		if (terminate)
			return;
		processStep = 'checkout';
		lg.callMain([processStep, event.data.targetBranch]);
		if (terminate)
			return;
		processStep = 'merge';
		lg.callMain([processStep, event.data.sourceBranch]);
		if (terminate)
			return;
		processStep = 'push';
		lg.callMain([processStep]);
		if (terminate)
			return;
		postMessage({state: 'merged'});
	};
	postMessage({state: 'ready'});
}
