<template>
  <div>
    <button :class="$style.button" @click="showModal = true" @mouseover="hover = true" @mouseleave="hover = false">
      <div v-if="hover" :class="$style.hoverContent">
        <img src="/images/download_lightblue.png" :alt="hoverAltText" :class="$style.icon">
        <p>{{ hoverText }}</p>
      </div>
      
      <div v-else :class="$style.defaultContent">
        <img src="/images/macOS_icon.png" :alt="defaultAltText" :class="$style.icon">
        <p>{{ defaultText }}</p>
      </div>
    </button>

    <div v-if="showModal" :class="$style.modalBackdrop" @click.self="showModal = false">
      <div :class="$style.modalContent">
        <h3>{{ modalTitle }}</h3>
        <button :class="$style.smallButton" @click="downloadFileX64">{{ buttonTextX64 }}</button>
        <button :class="$style.smallButton" @click="downloadFileArm">{{ buttonTextArm }}</button>
        <button :class="$style.closeButton" @click="showModal = false">{{ closeButtonText }}</button>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue';
import { useRoute } from 'vitepress';

const showModal = ref(false);
const hover = ref(false);
const route = useRoute();
const language = computed(() => route.path.startsWith('/zh/') ? 'zh' : 'en');

const hoverText = computed(() => language.value === 'zh' ? '点击下载' : 'Click to Download');
const defaultText = computed(() => language.value === 'zh' ? 'macOS' : 'macOS');
const hoverAltText = computed(() => language.value === 'zh' ? '下载' : 'Download');
const defaultAltText = computed(() => language.value === 'zh' ? 'macOS' : 'macOS');
const modalTitle = computed(() => language.value === 'zh' ? '选择下载版本' : 'Select Download Version');
const buttonTextX64 = computed(() => language.value === 'zh' ? '下载 macOS (x64)' : 'Download macOS (x64)');
const buttonTextArm = computed(() => language.value === 'zh' ? '下载 macOS (Arm64)' : 'Download macOS (Arm64)');
const closeButtonText = computed(() => language.value === 'zh' ? '关闭' : 'Close');

const version = 'v1.2.9.7';

function downloadFileX64() {
  const link = document.createElement('a');
  link.href = `https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/${version}/MoganResearch-${version}.dmg`;
  link.download = language.value === 'zh' ? `墨干${version}` : `Mogan ${version}`;
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
}

function downloadFileArm() {
  const link = document.createElement('a');
  link.href = `https://mirrors.ustc.edu.cn/github-release/XmacsLabs/mogan/${version}/MoganResearch-${version}-arm.dmg`;
  link.download = language.value === 'zh' ? `墨干${version}-arm` : `Mogan ${version}-arm`;
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
  color: #333;
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
}

.hoverContent {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  font-size: 18px;
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
  color: black;
  padding: 20px;
  border-radius: 10px;
  text-align: center;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);
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

@media (max-width: 600px) {
  .button {
    width: 100px;
    height: 100px;
    padding: 8px;
  }

  .icon {
    width: 40px;
    height: 40px;
  }

  .button p {
    font-size: 14px;
    margin-top: 5px;
  }

  .hoverContent {
    font-size: 16px;
  }
}

@media (prefers-color-scheme: dark) {
  .button {
    background-color: #333;
    color: white;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);
  }

  .modalContent {
    background-color: #444;
    color: white;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.7);
  }

  .hoverContent {
    color: black;
  }
  
  .smallButton, .closeButton {
    color: white;
  }
}
</style>