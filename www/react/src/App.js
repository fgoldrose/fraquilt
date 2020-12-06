import './App.css';
import React from 'react';
import {randFuncs, randColors} from './randomfuncs';
import spinner from './loading.gif';
import servererror from './servererror.png';
import generationerror from './generationerror.png';

const RED = 0;
const GREEN = 1;
const BLUE = 2;

function TextInput(props) {
  return (
    <input type="text" value={props.value} onChange={props.onChange} style={{width: '70px'}} />
    )
}

function NumInput(props) {
  return (
    <input type="number" value={props.value} min={0} style={{width: '50px'}}
    onChange={e => props.onChange(parseInt(e.target.value), 10)}/>
    )
}

class DisplayImage extends React.Component {
  constructor(props){
    super(props);
    this.state = {
      url: props.url,
      loading: false
    }
  }

  getImage = (url, n) => {
    if(n <= 0){
      this.setState(ps => ({...ps, loading: false, url: servererror}));
    }
    else{
      fetch(url, {method: 'HEAD'})
        .then(r => {
            if(r.status === 200){
                this.setState(ps => ({...ps, loading: false}));
            }
            else {
                setTimeout(() => this.getImage(url, n-1), 1000);
            }
      })
    }
  }

  processError = (err) => {
    this.setState(ps => ({...ps, loading: true}));
    this.getImage(this.props.url, 30);
  }

  render(){
    if(this.state.loading){
      return <img src={spinner} alt='' width={this.props.width} height={this.props.height}/>;
    }
    else{
      return <img src={this.state.url} onError={this.processError} alt='' width={this.props.width} height={this.props.height} style={{imageRendering: this.props.rendering}}/>;
    }
  }
}

function RecentImages(props){
  return (
    <div style={{'padding': '3px'}}>
      {props.imagelist.map((i) => (
        <a key={i} target="_blank" rel="noreferrer" href={i} style={{padding:'5px'}}>
          <DisplayImage url={i} width={100} height={100} rendering={'auto'}/>
        </a>))}
    </div>
    );
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
        <div style={{display: 'flex', alignItems: 'center', flexWrap: 'wrap', padding:'10px', border: '2px solid black'}}>
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
      height: props.height,
      iterations: props.iterations,
      colors: Array(props.numcolors).fill([255,255,255]),
      functions: this.resetFunctions(props.width, props.height, props.numcolors),
      url: "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==",
      recent: []
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
                          , colors: Array(n).fill([255,255,255])
                          , functions: this.resetFunctions(ps.width, ps.height, n)}));
  }

  changeWidth = (n) => {
    this.setState(ps => ({...ps
                          , width: n
                          , height: n
                          , functions: this.resetFunctions(n, n, ps.numcolors)}));
  }

  generateRandom = () => {
    let fs = randFuncs(this.state.numcolors, this.state.width, this.state.height);
    let cs = randColors(this.state.numcolors);
    this.setState(ps => ({...ps, colors: cs, functions: fs}), this.generateImage);
  }

  generateImage = () => {
    this.setState(ps => ({...ps, url: spinner}));
    let data = {
      "iterations": this.state.iterations,
      "colors": this.state.colors,
      "functions": this.state.functions
    }

    const url = '/api/'
    const req = {
        method: 'POST',
        headers : {'Content-Type' : 'application/json'},
        body: JSON.stringify(data)
     }

     fetch(url, req)
        .then(res => {
          if(res.status === 200){
               res.json().then(data => {
                if('image' in data){ //get base64 image to avoid waiting for s3 upload
                  this.setState(ps => ({...ps, url: data.image, recent: [data.url].concat(ps.recent)}))
                }
                else{
                  this.setState(ps => ({...ps, url: data.url, recent: [data.url].concat(ps.recent)}))
                }
              })      
          } 
          else if(res.status === 400){
            // expected result of invalid request
            this.setState(ps => ({...ps, url: generationerror}))
          }
          else {
            // unexpected error from server
              this.setState(ps => ({...ps, url: servererror}))
          }                   
      })
  }

  render() {
    let colorpickers = [];
    for(let i=0; i < this.state.numcolors; i++){
      colorpickers.push(<ColorInput key={i} i={i} value={this.rgbToHex(this.state.colors[i])} onChange={this.changeColors}/>)
    }

    return(
      <div style={{padding: '10px'}}>
        <div style={{display: 'flex', flexWrap: 'wrap'}}>
          <div style={{flex: "1 1 50%", margin:'5px'}}>
            <h2>Fraquilt</h2>
            <div style={{margin: '3px'}}>
            <label>Iterations</label>
            <NumInput key="iterations" value={this.state.iterations} onChange={this.changeIterations}/>
            </div>
            <div style={{margin: '3px'}}>
            <label>Number of colors</label>
            <NumInput key="numcolors" value={this.state.numcolors} onChange={this.changeNumColors}/>
            </div>
            <div style={{margin: '3px'}}>
            <label>Width</label>
            <NumInput key="width" value={this.state.width} onChange={this.changeWidth}/>
            </div>
            <div style={{margin: '3px'}}>{colorpickers}</div>  
            <Picker functions={this.state.functions} width={this.state.width} height={this.state.height} numcolors={this.state.numcolors} onChange = {this.changeFunctions}/>
            <button onClick={this.generateRandom}>Random</button>
            <button onClick={this.generateImage}>Generate</button>
          </div>
          <div style={{flexShrink: 0, flexGrow: 1}}>
            <DisplayImage url={this.state.url} key={this.state.url} width={600} height={600} rendering={'pixelated'}/>
          </div>
        </div>
        <br/>
        <label>Recently Created: </label>
        <RecentImages imagelist={this.state.recent}/>
      </div>
      )
  }
}

function App() {
  return (
    <div className="App">
      <Fraquilt width={2} height={2} numcolors={1} iterations={9}/>
    </div>
  );
}

export default App;
