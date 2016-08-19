const extra_map = (notebook, cell) => ({
	['Alt--'](cm) {
		cm.replaceSelection(' <- ')
	},
	['Shift-Ctrl-M'](cm) {
		cm.replaceSelection(' %>% ')
	},
	F1(cm) {
		const {anchor, head} = cm.findWordAt(cm.getCursor())
		const word = cm.getRange(anchor, head)
		
		const callbacks = cell.get_callbacks()
		const options = {silent: false, store_history: false, stop_on_error: true}
		cell.last_msg_id = notebook.kernel.execute(`help(${word})`, callbacks, options) 
	},
})

function enable_extra_keys(notebook, cell) {
	if (!cell.code_mirror) return
	const old_keymap = cell.code_mirror.getOption('extraKeys')
	const new_keymap = Object.assign({}, old_keymap, extra_map(notebook, cell))
	cell.code_mirror.setOption('extraKeys', new_keymap)
}

define(['base/js/namespace'], ({notebook}) => ({
	onload() {
		for (let cell of notebook.get_cells())
			enable_extra_keys(notebook, cell)
		
		notebook.events.on('edit_mode.Cell', (event, {cell}) =>
			enable_extra_keys(notebook, cell))
	},
}))
