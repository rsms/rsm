const fs = require("fs")
const vttparse = require("./_vttparse").parse

if (process.argv.length < 5)
  throw "usage: prog <infilevtt> <title> <outfilehtml>"

let INFILE = process.argv[2]
let TITLE = process.argv[3]
let OUTFILE_HTML = process.argv[4]

const parsed = vttparse(fs.readFileSync(INFILE, "utf8"), { strict: false })
// console.log(parsed)
// for (let cue of parsed.cues) {
//   console.log(cue.text)
// }

let endtime = 0
for (let cue of parsed.cues) {
  endtime = Math.max(endtime, cue.end)
}

let prevtext = ""
let seconds = 0
let lastsec = -9999999
let buf = []
let outplain = []
let outmd = []
let outhtml = []

function flush() {
  let time = seconds
  let hour = Math.floor(seconds / 3600); seconds %= 3600
  let min = Math.floor(seconds / 60); seconds = Math.floor(seconds % 60)
  let timestamp = ""
  if (endtime >= 60*60)
    timestamp += (hour + ":").padStart(3, '0')
  timestamp += (min + ":").padStart(3, '0')
  timestamp += String(seconds).padStart(2, '0')
  outplain.push(timestamp + ": " + buf.join(" "))
  outhtml.push(
    `<a class="time" name="t${time}" href="#t${time}">${timestamp}</a> ${buf.join(" ")}<br>`)
  outmd.push('_' + timestamp + ":_ " + buf.join(" ") + "<br>")
  buf = []
  seconds = 0
}

for (let cue of parsed.cues) {
  let text = cue.text.split("\n")[0].trim()
  if (text == prevtext)
    continue
  prevtext = text
  if (text == "")
    continue
  buf.push(text)
  if (seconds == 0)
    seconds = cue.start
  if (cue.start - lastsec > 2 && buf.length > 1)
    flush()
  lastsec = cue.start
}

// final lines
let lastcue = parsed.cues[parsed.cues.length - 1]
let text = lastcue.text.split("\n").slice(1).join(" ")
text = text.replace(/<[^>]+>/g, " ").replace(/ +/g, " ").trim()
if (!prevtext.startsWith(text))
  buf.push(text)
if (seconds == 0)
  seconds = lastcue.start
flush()

// console.log(outplain.join("\n"))
// console.log(outhtml.join("\n"))
// console.log(outmd.join("\n"))

let html = `---
layout: simple2
title: "Transcript of RSM ${TITLE}"
---

<h1>{{page.title}}</h1>
<p><a href="./">RSM project page</a></p>
<p>
${outhtml.join("\n")}
</p>
`

fs.writeFileSync(OUTFILE_HTML, html, "utf8")
