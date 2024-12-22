<template>
    <div>
      <button :class="$style.button" @click="showModal = true" @mouseover="hover = true" @mouseleave="hover = false">
        <div v-if="hover" :class="$style.hoverContent">
          <img src="/images/download_lightblue.png" alt="download" :class="$style.icon">
          <p>Click to download</p>
        </div>
        
        <div v-else :class="$style.defaultContent">
          <img src="/images/macOS_icon.png" alt="macOS" :class="$style.icon">
          <p>macOS</p>
        </div>
      </button>
  
      <div v-if="showModal" :class="$style.modalBackdrop" @click.self="showModal = false">
        <div :class="$style.modalContent">
          <h3>Select Download Version</h3>
          <button :class="$style.smallButton" @click="downloadFileX64">Download macOS (x64)</button>
          <button :class="$style.smallButton" @click="downloadFileArm">Download macOS (Arm64)</button>
          <button :class="$style.closeButton" @click="showModal = false">Close</button>
        </div>
      </div>
    </div>
  </template>
  
  <script setup>
  import { ref } from 'vue';
  
  const showModal = ref(false);
  const hover = ref(false);
  
  function downloadFileX64() {
    const link = document.createElement('a');
    link.href = 'https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7.dmg';
    link.download = 'mogan v1.2.9.7';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  }
  
  function downloadFileArm() {
    const link = document.createElement('a');
    link.href = 'https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/MoganResearch-v1.2.9.7-arm.dmg';
    link.download = 'mogan v1.2.9.7-arm';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  }
  </script>
  
  <style module>
  .button {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    width: 150px;
    height: 150px;
    margin: 10px;
    padding: 10px;
    text-align: center;
    border-radius: 10px;
    background-color: white;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
    transition: transform 0.2s, background-color 0.2s;
    cursor: pointer;
  }
  
  .button:hover {
    transform: scale(1.05);
    background-color: #f0f0f0;
  }
  
  .button:active {
    transform: scale(0.95);
    background-color: #e0e0e0;
  }
  
  .defaultContent {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
  }
  
  .icon {
    width: 60px;
    height: 60px;
    object-fit: contain;
  }
  
  .button p {
    margin-top: 10px;
    font-size: 16px;
    color: #333;
  }
  
  .hoverContent {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    font-size: 18px;
    color: #A5C9F8;
  }
  
  .modalBackdrop {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(0, 0, 0, 0.5);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 1000;
  }
  
  .modalContent {
    background: white;
    padding: 20px;
    border-radius: 10px;
    text-align: center;
  }
  
  .smallButton {
    margin: 10px;
    padding: 5px 10px;
    font-size: 14px;
    color: white;
    background-color: #A5C9F8;
    border: none;
    border-radius: 5px;
    cursor: pointer;
    transition: background-color 0.2s;
  }
  
  .smallButton:hover {
    background-color: #0056b3;
  }
  
  .smallButton:active {
    background-color: #003f7f;
  }
  
  .closeButton {
    margin-top: 20px;
    padding: 5px 10px;
    font-size: 14px;
    color: white;
    background-color: #A5C9F8;
    border: none;
    border-radius: 5px;
    cursor: pointer;
    transition: background-color 0.2s;
  }
  
  .closeButton:hover {
    background-color: #0056b3;
  }
  
  .closeButton:active {
    background-color: #003f7f;
  }
  </style>
  