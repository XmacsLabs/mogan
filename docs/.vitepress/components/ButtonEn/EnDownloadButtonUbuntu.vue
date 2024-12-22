<template>
    <div>
      <button :class="$style.button" @click="showModal = true" @mouseover="hover = true" @mouseleave="hover = false">
        <div v-if="hover" :class="$style.hoverContent">
          <img src="/images/download_orange.png" alt="download" :class="$style.icon">
          <p>Click to download</p>
        </div>
        
        <div v-else :class="$style.defaultContent">
          <img src="/images/Ubuntu_icon.png" alt="ubuntu" :class="$style.icon">
          <p>Ubuntu</p>
        </div>
      </button>
  
      <div v-if="showModal" :class="$style.modalBackdrop" @click.self="showModal = false">
        <div :class="$style.modalContent">
          <h3>Select Download Version</h3>
          <button :class="$style.smallButton" @click="downloadFile2204">Download Ubuntu 22.04</button>
          <button :class="$style.smallButton" @click="downloadFile2404">Download Ubuntu 24.04</button>
          <button :class="$style.closeButton" @click="showModal = false">Close</button>
        </div>
      </div>
    </div>
  </template>
  
  <script setup>
  import { ref } from 'vue';
  
  const showModal = ref(false);
  const hover = ref(false);
  
  function downloadFile2204() {
    const link = document.createElement('a');
    link.href = 'https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-ubuntu22.04.deb';
    link.download = 'moganv1.2.9.7';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  }
  
  function downloadFile2404() {
    const link = document.createElement('a');
    link.href = 'https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/v1.2.9.7/mogan-research-v1.2.9.7-ubuntu24.04.deb';
    link.download = 'moganv1.2.9.7';
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
    color: #ea4f06;
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
    background-color: #ea4f06;
    border: none;
    border-radius: 5px;
    cursor: pointer;
    transition: background-color 0.2s;
  }
  
  .smallButton:hover {
    background-color: #cc0000;
  }
  
  .smallButton:active {
    background-color: #990000;
  }
  
  .closeButton {
    margin-top: 20px;
    padding: 5px 10px;
    font-size: 14px;
    color: white;
    background-color: #ea4f06;
    border: none;
    border-radius: 5px;
    cursor: pointer;
    transition: background-color 0.2s;
  }
  
  .closeButton:hover {
    background-color: #cc0000;
  }
  
  .closeButton:active {
    background-color: #990000;
  }
  </style>
  
  
  