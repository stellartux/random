const access = await navigator.requestMIDIAccess({ sysex: true })
const midiOutputSelect = document.getElementById('midi-outputs')
const octaveTranspose = document.getElementById('octave-transpose')
const semitoneTranspose = document.getElementById('semitone-transpose')

function transposeAdjust(pitch) {
  return pitch + Number(semitoneTranspose.value) + Number(octaveTranspose.value) * 12
}

let midiOutput
if (access.outputs.length === 1) {
  midiOutput = access.outputs[0][1]
}
window.keymap = null

function reloadOutputs() {
  while (midiOutputSelect.length > 1) {
    midiOutputSelect.removeChild(midiOutputSelect.lastChild)
  }
  for (const output of access.outputs) {
    const option = document.createElement('option')
    option.value = output[0]
    option.innerText = output[1].name
    midiOutputSelect.append(option)
  }
}
reloadOutputs()

document.getElementById('refresh-outputs').addEventListener('click', reloadOutputs)

midiOutputSelect.onchange = () => {
  midiOutput = access.outputs.get(midiOutputSelect.value)
}

/**
 * Loads a JSON file representing a keymap
 * @param {string} url the URL of the keymap JSON
 */
async function loadKeymap(url) {
  const file = await fetch(url)
  keymap = await file.json()
}
loadKeymap('piano-keymap.json')
document.getElementById('keymap-select').addEventListener('change', ({target: { value }}) => loadKeymap(value))

document.addEventListener('keydown', (event) => {
  if (!event.repeat && Object.keys(keymap).includes(event.code)) {
    noteOn(transposeAdjust(keymap[event.code]))
  }
})

document.addEventListener('keyup', (event) => {
  if (Object.keys(keymap).includes(event.code)) {
    noteOff(transposeAdjust(keymap[event.code]))
  }
})

document.getElementById('keymap-file').addEventListener('change', (event) => {
  const reader = new FileReader()
  reader.readAsText(event.target.files[0])
  reader.onload = () => keymap = JSON.parse(reader.result)
})

function noteOn(pitch, velocity = 100, channel = 0) {
  console.log(`Noteon: ${pitch}`)
  midiOutput.send([0x90 | (channel & 0xf), 0x7f & pitch, 0x7f & velocity])
}

function noteOff(pitch, channel = 0) {
  console.log(`Noteoff: ${pitch}`)
  midiOutput.send([0x80 | (channel & 0xf), 0x7f & pitch, 0])
}
