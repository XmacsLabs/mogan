<template>
    <div>
      <button :class="$style.button" @click="showModal = true">View SHA256 checksum</button>
  
      <div v-if="showModal" :class="$style.modalBackdrop" @click.self="showModal = false">
        <div :class="$style.modalContent">
          <h3>SHA256 checksum</h3>
          <pre>
  3eeb3f15b930110b22febe3a27e937efaceaa7821d62c6b73a1f17484825dc46  MoganResearch-v1.2.9.7-64bit-installer.exe
  4b2dbfe74492031f12760701c5f878b98bb1fb2abc2e449f7d692ba2dc4b9939  MoganResearch-v1.2.9.7-arm.dmg
  a31276e0091b008e3ff9a2f3f1a6e00828221371fe073cb53d2760d349af4fa0  MoganResearch-v1.2.9.7.dmg
  f4bcb8d4a9bec63b158c93fc0efbc083da95de84ddf7fb84ecf56675120458e5  mogan-research-v1.2.9.7-ubuntu22.04.deb
  6716064ec9f570aad3cbd8a84251d06cbbc7055f4875b323d2c7718a39ac9231  mogan-research-v1.2.9.7-ubuntu24.04.deb
          </pre>
          <button :class="$style.closeButton" @click="showModal = false">Close</button>
        </div>
      </div>
    </div>
  </template>
  
  <script setup>
  import { ref } from 'vue';
  
  const showModal = ref(false);
  </script>
  
  <style module>
  .button {
    color: white;
    background-color: #007bff;
    padding: 10px 20px;
    border: none;
    border-radius: 5px;
    font-weight: bold;
    cursor: pointer;
    transition: background-color 0.3s, transform 0.3s;
  }
  
  .button:hover {
    background-color: #0056b3;
  }
  
  .button:active {
    background-color: #003f7f;
    transform: scale(0.95);
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
    text-align: left;
    max-width: 90%;
  }
  
  .closeButton {
    margin-top: 20px;
    padding: 5px 10px;
    font-size: 14px;
    color: white;
    background-color: #ff4d4d;
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
  