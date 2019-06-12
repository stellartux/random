class Display {
  constructor (parent, n) {
    this.elem = document.createElement('div')
    this.count = n
    this.elem.classList.add('display')
    let colour = document.createElement('span')
    let checkbox = document.createElement('input')
    checkbox.type = 'checkbox'
    if (n === 'c') {
      checkbox.disabled = true
    } else {
      this.elem.onmouseup = () => {
        window.requestAnimationFrame(() => {
          let target = inputs[Number(this.count === 'b')]
          target.value = this.value
          updateDisplays()
        })
      }
    }
    let container = document.createElement('label')
    container.appendChild(checkbox)
    container.appendChild(colour)
    for (let i = 0; i < 8; i++) {
      this.elem.appendChild(container.cloneNode(true))
    }
    parent.appendChild(this.elem)
  }
  get value () {
    let count = 0
    for (let i = 0; i < this.elem.childElementCount; i++) {
      count <<= 1
      if (this.elem.children[i].children[0].checked) {
        count++
      }
    }
    return count
  }
  set value (v) {
    for (let i = 8; i--;) {
      this.elem.children[7 - i].children[0].checked = v & 1 << i
    }
  }
}

let inputs
const displays = [],
  funs = {
    'bit-or': (a, b) => a | b,
    'bit-and': (a, b) => a & b,
    'bit-xor': (a, b) => a ^ b,
    'and': (a, b) => a | 0 && b | 0,
    'or': (a, b) => a | 0 || b | 0,
    'not': a => Number(a == 0)
  }

window.onload = () => {
  let main = document.getElementById('main')
  for (let i = 3; i--;) {
    displays.push(new Display(main, 'cba'[i]))
  }
  inputs = ['a', 'b', 'ops'].map(x => document.querySelector(`[name=${x}]`))
  for (let input of inputs) {
    input.addEventListener('input', updateDisplays)
  }
}

function updateDisplays () {
  document.querySelector('[name="c"]').value =
    displays[2].value = funs[inputs[2].value](
    displays[0].value = inputs[0].value,
    displays[1].value = inputs[1].value)
}
