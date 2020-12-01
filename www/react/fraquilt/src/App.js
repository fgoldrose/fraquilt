import logo from './logo.svg';
import './App.css';
import React from 'react';

function TextInput(props) {
  return (
    <input type="text" value={props.value} onChange={(event) => props.onChange(event.target.value)}/>
    )
}

class Picker extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      numrows: props.numrows,
      numcols: props.numcols,
      funcs: Array(props.numrows*props.numcols).fill(""),
    };
  }

  handleInput(i, newval) {
    const funcs = this.state.funcs.slice();
    funcs[i] = newval;
    this.setState({funcs: funcs});
  }

  renderInput(i) {
    return(
      <TextInput
        value = {this.state.funcval}
        onChange = {(newval) => this.handleInput(i, newval)}
      />
    )
  }

  render() {
    const funcinputs = [];
    var i = 0;
    for (var row = 0; row < this.state.numrows; row++) {
      const rowinputs = [];
      for (var col = 0; col < this.state.numcols; col++) {
        rowinputs.push(this.renderInput(i));
        i++;
      }
      funcinputs.push(<div>{rowinputs}</div>)
    }

    return(
      <div>
        {funcinputs}
      </div>
    )
  }
}

function App() {
  return (
    <div className="App">
      <Picker numrows={2} numcols={2}/>
    </div>
  );
}

export default App;
