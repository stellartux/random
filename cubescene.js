class Box {
  constructor(x,z,w) {
      this.x = x * w
      this.z = z * w
      this.width = w
      this.height = this.newHeight()
  }

  render(f) {
    push()
      translate(this.x, -1, this.z)
      box(this.width, this.height * (sin(this.x*millis()/160000) + cos(this.z*millis()/128000)), this.width)
    pop()
  }

  newHeight() {
    this.height = 2*(this.width + random() * this.width)/3
  }
}

const boxes = []

function setup(){
  createCanvas(window.innerWidth, window.innerHeight, WEBGL)

  const boxwidth = 30
  const cubecount = 8
  for (let z = 0; z <= cubecount; z++) {
    for (let x = 0; x <= cubecount; x++) {
      let b = new Box(x, z, boxwidth)
      boxes.push(b)
    }
  }
  noStroke()
  for (let b of boxes) {
    b.newHeight()
  }
}


function draw() {
  clear()
  let radius = 110
  let lh = 1
  let t = millis()/500
  let v = 2*(1.5+sin(t))*.25
  let cr = 200

  camera(120+cr*sin(t/20), 120, 120+cr*cos(t/20), 120, 0, 120, 0, -1, 0);
//  camera(0, 100, 0, 120, 0, 120, 0, -1, 0);

  pointLight(color(200, 0, 0), radius * sin(v), lh, radius * cos(v))
  pointLight(color(0, 200, 0), radius * sin(.333 + v), lh, radius * cos(.333 + v))
  pointLight(color(0, 0, 200), radius * sin(.666 + v), lh, radius * cos(.666 + v))
  pointLight(color(80, 0, 0), radius * sin(t), lh + 20*(sin(millis()/380)), radius * cos(t))
  pointLight(color(0, 80, 0), radius * sin(2*PI/3 + t), lh + 20*(sin(millis()/400)), radius * cos(2*PI/3 + t))
  pointLight(color(0, 0, 80), radius * sin(4*PI/3 + t), lh + 20*(sin(millis()/420)), radius * cos(4*PI/3 + t))

/*
  push()
  fill(color(255,255,0))
  translate(radius * sin(millis()/500), lh + 20*(sin(millis()/420)), radius * cos(millis()/500))
  sphere(10)
  pop()
  push()
  fill(color(255,0,255))
  translate(radius * sin(2*PI/3 + millis()/500), lh + 20*(sin(millis()/420)), radius * cos(2*PI/3 + millis()/500))
  sphere(10)
  pop()
  push()
  fill(color(0,255,255))
  translate(radius * sin(4*PI/3 + millis()/500), lh + 20*(sin(millis()/420)), radius * cos(4*PI/3 + millis()/500))
  sphere(10)
  pop()
*/

  directionalLight(color(225), 200, 50, 20)
  directionalLight(color(215), 0, 600, 0)
  directionalLight(color(45), 0, 60, -300)
  ambientLight(color(10))

  specularMaterial(215)
  push()
  translate(120,0,120)
  box(320, 1, 320)
  pop()

  let f = frameCount/10000
  for (let b of boxes) {
    b.render(f)
  }
}

function mouseClicked () {
  for (let b of boxes) {
    b.newHeight()
  }
}
