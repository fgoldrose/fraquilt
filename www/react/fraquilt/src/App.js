import './App.css';
import React from 'react';

const RED = 0;
const GREEN = 1;
const BLUE = 2;

function TextInput(props) {
  return (
    <input type="text" value={props.value} onChange={props.onChange}/>
    )
}

function NumInput(props) {
  return (
    <input type="number" value={props.value} onChange={e => props.onChange(parseInt(e.target.value), 10)}/>
    )
}

function ColorInput(props){
  return (
    <input type="color" value={props.value} onChange={e => props.onChange(e.target.value, props.i)}/>
    )
}

function ColorFuncs(props){
  return (
    <div style={{display: 'flex', flexDirection: 'column', padding:'5px', margin:'2px', border: '1px solid black'}}>
      <TextInput key="red" value={props.values[RED]} onChange={e => props.onChange(props.k, e.target.value, RED )}/>
      <TextInput key="green" value={props.values[GREEN]} onChange={e => props.onChange(props.k, e.target.value, GREEN )}/>
      <TextInput key="blue" value={props.values[BLUE]} onChange={e => props.onChange(props.k, e.target.value, BLUE )}/>
    </div>
    )
}

class Division extends React.Component {

  handleInput = (c, newval, color) => {
    let ret = this.props.values;
    ret[c][color] = newval;
    return this.props.onChange(this.props.x, this.props.y, ret);
  }

  render(){
    const colorfuncs = [];
    for (let c = 0; c < this.props.numcolors; c++) {
        colorfuncs.push(<ColorFuncs key={c} k={c} values={this.props.values[c]} onChange={this.handleInput}/>);
    }

      return (
        <div style={{display: 'flex', flexWrap: 'wrap', padding:'10px', border: '2px solid black'}}>
          {colorfuncs}
        </div>
        )
      }
}

class Picker extends React.Component {

  renderAll(){
    let funcinputs = [];
    let i = 0;
    for (let row = 0; row < this.props.width; row++) {
      const rowinputs = [];
      for (let col = 0; col < this.props.height; col++) {
        rowinputs.push(<Division key={i}
                        x = {row}
                        y = {col}
                        numcolors={this.props.numcolors}
                        values = {this.props.functions[row][col]}
                        onChange = {this.props.onChange}
                      />
        );
        i++;
      }
      funcinputs.push(<div key={row} style={{display: 'flex'}}>{rowinputs}</div>);
    }
    return funcinputs
  }

  render() {
    return(
      <div style={{display: 'flex', flexDirection: 'column', alignItems:'center'}}>
        {this.renderAll()}
      </div>
    )
  }
}

class Fraquilt extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      numcolors: props.numcolors,
      width: props.width,
      height: props.numcolors,
      iterations: props.iterations,
      colors: Array(props.numcolors).fill([255,255,255]),
      functions: this.resetFunctions(props.width, props.height, props.numcolors),
      url: ""
    };
  }

  resetFunctions(w, h, cs) {
    let emptyfunctions = []
    for(let x=0; x < w; x++){
      let row = []
      for(let y=0; y < h; y++){
        let divval = []
        for(let c=0; c < cs; c++){
          divval.push(["r" + c.toString(),"g" + c.toString(),"b" + c.toString()])
        }
        row.push(divval);
      }
      emptyfunctions.push(row);
    }
    return emptyfunctions;
  }

  hexToRGB(hex) {
    var bigint = parseInt(hex.slice(1), 16);
    var r = (bigint >> 16) & 255;
    var g = (bigint >> 8) & 255;
    var b = bigint & 255;

    return [r,g,b];
  }

  rgbToHex(color) {
    let r = color[RED];
    let b = color[BLUE];
    let g = color[GREEN];

    return '#' + [r, g, b].map(x => {
      const hex = x.toString(16);
      return hex.length === 1 ? '0' + hex : hex
    }).join('');
  }

  changeColors = (c, i) => {
    const colors = this.state.colors.slice();
    colors[i] = this.hexToRGB(c);
    this.setState(ps => ({...ps, colors: colors}))
  }

  changeFunctions = (x, y, newvals) => {
    const functions = this.state.functions.slice();
    functions[x][y] = newvals;
    this.setState(prevState => ({...prevState, functions: functions}));
  }

  changeIterations = (n) => {
    this.setState(ps => ({...ps, iterations: n}))
  }

  changeNumColors = (n) => {
    this.setState(ps => ({...ps
                          , numcolors: n
                          , colors: Array(n).fill(["255", "255", "255"])
                          , functions: this.resetFunctions(ps.width, ps.height, n)}));
  }

  changeWidth = (n) => {
    this.setState(ps => ({...ps
                          , width: n
                          , height: n
                          , functions: this.resetFunctions(n, n, ps.numcolors)}));
  }

  generateImage = () => {
    let data = {
      "iterations": this.state.iterations,
      "colors": this.state.colors,
      "functions": this.state.functions
    }
    console.log(JSON.stringify(data))

    const url = 'http://localhost:5000/api'
    const req = {
        method: 'POST',
        headers : {'Content-Type' : 'application/json'},
        body: JSON.stringify(data)
     }

     fetch(url, req)
        .then(res => {
          if(res.status == 200){
               res.json().then(this.getImage)      
          } 
          else {
              this.setState(ps => ({...ps, src: ""}))
          }                   
      })
  }

  getImage = (data) => {
    fetch(data.url, {method: 'HEAD'})
      .then(r => {
          if(r.status == 200){
              this.setState(ps => ({...ps, url: data.url}))
          }
          else {
              setTimeout(() => this.getImage(data), 500);
          }
    })
  }

  render() {
    let colorpickers = [];
    for(let i=0; i < this.state.numcolors; i++){
      colorpickers.push(<ColorInput key={i} i={i} value={this.rgbToHex(this.state.colors[i])} onChange={this.changeColors}/>)
    }

    return(
      <div>
      <label>Iterations</label>
      <NumInput key="iterations" value={this.state.iterations} onChange={this.changeIterations}/>
      <label>Number of colors</label>
      <NumInput key="numcolors" value={this.state.numcolors} onChange={this.changeNumColors}/>
      <label>Width</label>
      <NumInput key="width" value={this.state.width} onChange={this.changeWidth}/>
      <div>{colorpickers}</div>
      <Picker functions={this.state.functions} width={this.state.width} height={this.state.height} numcolors={this.state.numcolors} onChange = {this.changeFunctions}/>
      <button onClick={this.generateImage}>Generate</button>
      <br/>
      <img src={this.state.url} width={600} heigh={600} style={{imageRendering: 'pixelated'}} />
      </div>
      )
  }
}

function App() {
  return (
    <div className="App">
      <Fraquilt width={2} height={2} numcolors={2} iterations={9}/>
    </div>
  );
}

export default App;
