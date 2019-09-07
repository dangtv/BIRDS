import React from 'react';
import './FoldingCube.css';

// http://tobiasahlin.com/spinkit/
export default function FoldingCube() {
  return (
    <div class="sk-folding-cube">
      <div class="sk-cube1 sk-cube" />
      <div class="sk-cube2 sk-cube" />
      <div class="sk-cube4 sk-cube" />
      <div class="sk-cube3 sk-cube" />
    </div>
  );
}
