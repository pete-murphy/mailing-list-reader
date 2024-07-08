const body = document.querySelector("body");
const ul = document.createElement("ul");
body.appendChild(ul);

let allReplyTo = new Set();
let allMessageIds = new Set();
let errored = new Set();

function appendLi(json) {
  const {
    content,
    author,
    date,
    subject,
    inReplyTo: _inReplyTo,
    messageID,
  } = JSON.parse(json);

  const inReplyTo = _inReplyTo?.split(" ").at(0);

  allReplyTo.add(inReplyTo);
  allMessageIds.add(messageID);

  const li = document.createElement("li");
  li.dataset.messageId = messageID;
  li.dataset.inReplyTo = inReplyTo;

  const h2 = document.createElement("h2");
  h2.innerText = subject;

  const address = document.createElement("address");
  address.innerHTML = author;

  const time = document.createElement("time");
  time.innerHTML = date;
  time.dateTime = date;

  const header = document.createElement("header");
  header.appendChild(h2);
  header.appendChild(address);
  header.appendChild(time);

  const article = document.createElement("article");
  article.innerText = content;

  li.appendChild(header);
  // li.appendChild(article);

  if (inReplyTo) {
    const parent = document.querySelector(`li[data-message-id="${inReplyTo}"]`);
    console.assert(
      parent,
      `Parent not found for ${messageID}`,
      author,
      subject
    );
    if (parent) {
      const innerUL = parent.querySelector("ul");
      if (!innerUL) {
        const innerUL = document.createElement("ul");
        parent.appendChild(innerUL);
        innerUL.appendChild(li);
      } else {
        innerUL.appendChild(li);
      }
    } else {
      ul.appendChild(li);
    }
  } else {
    ul.appendChild(li);
  }
}

// The following code is from:
// https://stackoverflow.com/questions/76315479/split-a-readablestream-into-lines

function concatArrayBuffers(chunks) {
  const result = new Uint8Array(chunks.reduce((a, c) => a + c.length, 0));
  let offset = 0;
  for (const chunk of chunks) {
    result.set(chunk, offset);
    offset += chunk.length;
  }
  return result;
}

class LineSplitter extends TransformStream {
  _buffer = [];

  constructor() {
    super({
      transform: (chunk, controller) => {
        let index;
        let rest = chunk;
        while ((index = rest.indexOf(0x0a)) !== -1) {
          controller.enqueue(
            concatArrayBuffers([...this._buffer, rest.slice(0, index + 1)])
          );
          rest = rest.slice(index + 1);
          this._buffer = [];
        }

        if (rest.length > 0) {
          this._buffer.push(rest);
        }
      },
      flush: (controller) => {
        if (this._buffer.length > 0) {
          controller.enqueue(concatArrayBuffers(this._buffer));
        }
      },
    });
  }
}
fetch("data.ndjson").then((res) =>
  res.body
    .pipeThrough(new LineSplitter())
    .pipeThrough(new TextDecoderStream())
    .pipeTo(
      new WritableStream({
        async write(chunk) {
          await new Promise((resolve) => requestAnimationFrame(resolve));
          try {
            appendLi(chunk);
          } catch (error) {
            errored.add(chunk);
            console.error(error);
            console.log(allMessageIds);
            console.log(allReplyTo);
            if (errored.size > 8) {
              console.log("errored", errored);
              throw new Error("Too many errors");
            }
          }
        },
      })
    )
);
