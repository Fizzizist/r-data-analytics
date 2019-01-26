var dataSaved = true;

Shiny.addCustomMessageHandler("setSavedHandler", setSaved);

window.onbeforeunload = function(event) {
	if(!dataSaved){
		event.returnValue = " ";
	}
};

function setSaved(saved){
	dataSaved = saved;
}
