const eventRecord = {} // 'eventType' : [Callbacks]

class OurButton {

    constructor(text) {
        this.text = text;
    }

    add_EventListener (eventType, newCallback){
        if(!eventRecord[eventType])
            eventRecord[eventType] = []
        
        eventRecord[eventType].push(newCallback);
            
    }

    changeText(newText) {
        this.text = newText;
    }


}


function executeEvent(eventType) {

    const eventCallbacks = eventRecord[eventType];

    for(let callback of eventCallbacks) {
        callback(eventType);
    }
}

let ourButton = new OurButton("Click Me");
let newText = "Click this button";
ourButton.add_EventListener('click', (e) => ourButton.changeText(newText)); // We defined func here... 
console.log(ourButton.text);
executeEvent('click')
console.log(ourButton.text);