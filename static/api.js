console.log("Demo API");

let socket = new WebSocket("ws://localhost:8000/hydra/api");
let count = 0;

socket.onopen = e => {
    console.log("Opened");
    getDevnetAddresses(2);
};

socket.onmessage = event => {
    console.log("Message: ", event.data);
};

socket.onclose = event => {
    console.log("Close");
};

socket.onerror = error => {
    console.log("Error");
};

function getDevnetAddresses(amount)
{
    let payload = { tag: "GetDevnetAddresses",  contents: 3 };
    let taggedPayload = { tagged_payload: payload, tagged_id: 0 };

    console.log("Payload: ", JSON.stringify(payload));
    console.log("Tagged Payload: ", JSON.stringify(taggedPayload));
    socket.send(JSON.stringify(taggedPayload));
}

// getDevnetAddresses(2);
