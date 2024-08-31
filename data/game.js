const urlParams = new URLSearchParams(window.location.search);
const room = urlParams.get('room');
const mode = urlParams.get('mode');
const user = urlParams.get('user');

displayfield = document.querySelector("#playfield");
overfield = document.querySelector("#overlay");
statusbox = document.querySelector("#status");
playfield = [];
overlay = [];

let width = 80;
let height = 80;
let dr = 80;


function toHTML(html) {
    let temp = document.createElement('template');
    html = html.trim();
    temp.innerHTML = html;
    return temp.content.firstChild;
}


function draw_svg(ix, raw, col) {
    let [i, j] = ix;
    let cell = playfield[i][j];
    let im = toHTML(raw);
    
    im.setAttribute("class", "square");
    im.setAttribute("fill", col);
    
    cell.innerHTML = "";
    cell.appendChild(im);
}


function draw_text(cell, shape, colour) {
    cell.innerHTML = "";
    cell.innerHTML = shape;
    
    cell.style.color = colour;
    
    let children = cell.children;
    
    if (children.length > 1)
    {
        cell.removeChild(children[0]);
    }
}
    

let processWaitlist = Promise.resolve(0);

function process(effect, args) {
    console.log(effect, args);

    switch (effect) {
        case "MarkAvailableMove": {
            let [j, i] = args;

            let cell = overlay[i][j]; // so realistically this should use a different overlay now
            cell.classList.add("legal-move");

            break;
        }
        case "ClearAvailableMoves": {
            for (let i = 0; i < 8; ++i) {
                for (let j = 0; j < 8; ++j) {
                    let cell = overlay[i][j];
                    cell.classList.remove("legal-move");
                }
            }

            break;
        }
        case "Tile": {
            let [[j, i], piece, colour] = args;
        
            shape = "";
            
            if (piece) {
                [shape, _] = piece;
            }
            
            let cell = playfield[i][j];
            
            if (shape.endsWith(".svg")) {
                let draw_callback = _ => fetch("/images/" + shape).then(response => response.text()).then(raw => draw_svg([i, j], raw, colour));
                processWaitlist = processWaitlist.then(draw_callback);
            }
            else {
                let draw_callback = _ => draw_text(cell, shape, colour);
                processWaitlist = processWaitlist.then(draw_callback);
            }
            break;
        }
        case "Board": {
            for (const tile of args) {
                let [[j, i], [shape, colour]] = tile;
                shape = shape ? shape : "";
                let cell = playfield[i][j];
                
                if (shape.endsWith(".svg")) {
                    let draw_callback = _ => fetch("/images/" + shape).then(response => response.text()).then(raw => draw_svg([i, j], raw, colour));
                    processWaitlist = processWaitlist.then(draw_callback);
                }
                else {
                    let draw_callback = _ => draw_text(cell, shape, colour);
                    processWaitlist = processWaitlist.then(draw_callback);
                }
            }
            break;
        }
        case "config": {
            let [key, value] = args;
            
            if (key === "board_size") {
                let [m, n] = value;
                createBoard(n, m);
            }
            break;
        }
        case "Status": {
            statusbox.innerHTML = args;
            break;
        }
        case "SelectTile": {
            let [[j, i], a] = args;
            
            cell = overlay[i][j];
            if (a) {
                let text = playfield[i][j].innerHTML.fontcolor("0xFF0000");
                console.log(text);
                cell.innerHTML = text;
            }
            else
            {
                cell.innerHTML = ""
            }

            let children = cell.children;
        
            if (children.length > 1)
            {
                cell.removeChild(children[0]);
            }

            break;
        }
        case "Promotion": {
            let resp = prompt(args);
            socket.send(aesonEncode(resp, "PromoteMsg"));
            break;
        }
    }
}

host = window.location.hostname
socket = new WebSocket("ws://" + host + ":58846");
socket.onmessage = function (event) {
    let msg = event.data;
    f = function (txt) {
        let data = JSON.parse(txt);
        process(data["tag"], data["contents"]);
    }
    msg.text().then(f);
};
    
socket.onopen = function (_) {
    socket.send(aesonEncode([room, user], "Register"));
};


function aeson(contents, tag) {
    return {"contents": contents, "tag": tag};
}

function aesonEncode(contents, tag) {
    return JSON.stringify(aeson(contents, tag));
}



function createBoard(n, m) {
    //let stylelink = document.querySelector("#stylesheet");
    //let sheet = stylelink.sheet;
    //let rules = sheet.rules;
    
    for (let i = 0; i < n; i++) {
        let row = document.createElement("div");
        row.classList.add("row");
        row.classList.add("chess-row");
        displayfield.appendChild(row);

        let orow = document.createElement("div");
        orow.classList.add("row");
        orow.classList.add("chess-row");
        overfield.appendChild(orow);

        playfield.push([]);
        overlay.push([]);
        for (let j = 0; j < m; j++) {
            let cell = document.createElement("div");
            cell.classList.add("col");
            row.appendChild(cell);
            
            let ocell = document.createElement("div");
            ocell.classList.add("col");
            orow.appendChild(ocell);


            if ((i + j) % 2 === 1)
                cell.classList.add("whitetile");
            else
                cell.classList.add("blacktile");

            cell.onclick = function (_) {
                socket.send(aesonEncode([j, i], "TouchMsg"));
            };

            overlay[i].push(ocell);
            playfield[i].push(cell);
        }
    }
}

createBoard(8,8);