const extra_map = (Jupyter, cell) => ({
	['Alt--'](cm) {
		cm.replaceSelection(' <- ')
	},
	F1(cm) {
		const {anchor, head} = cm.findWordAt(cm.getCursor())
		const word = cm.getRange(anchor, head)
		
		const callbacks = cell.get_callbacks()
		const options = {silent: false, store_history: false, stop_on_error: true}
		cell.last_msg_id = Jupyter.notebook.kernel.execute(`help(${word})`, callbacks, options) 
	},
})

define(['base/js/namespace'], Jupyter => ({
	onload() {
		for (let cell of Jupyter.notebook.get_cells()) {
			cell.code_mirror.setOption('extraKeys', extra_map(Jupyter, cell))
		}
	},
}))
