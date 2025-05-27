<template>
  <div>
    <button :class="$style.button" @click="showModal = true">{{ buttonText }}</button>

    <div v-if="showModal" :class="$style.modalBackdrop" @click.self="showModal = false">
      <div :class="$style.modalContent">
        <h3>{{ modalTitle }}</h3>
        <pre>{{ checksums }}</pre>
        <button :class="$style.closeButton" @click="showModal = false">{{ closeButtonText }}</button>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue';
import { useRoute } from 'vitepress';

const version = 'v1.2.9.7';  // 定义版本号

const route = useRoute();
const language = computed(() => route.path.startsWith('/zh/') ? 'zh' : 'en');

const showModal = ref(false);

const buttonText = computed(() => language.value === 'zh' ? '查看 SHA256 校验码' : 'View SHA256 Checksum');
const modalTitle = computed(() => language.value === 'zh' ? 'SHA256 校验码' : 'SHA256 Checksum');
const closeButtonText = computed(() => language.value === 'zh' ? '关闭' : 'Close');

const checksums = computed(() => `
3eeb3f15b930110b22febe3a27e937efaceaa7821d62c6b73a1f17484825dc46  MoganResearch-${version}-64bit-installer.exe
4b2dbfe74492031f12760701c5f878b98bb1fb2abc2e449f7d692ba2dc4b9939  MoganResearch-${version}-arm.dmg
a31276e0091b008e3ff9a2f3f1a6e00828221371fe073cb53d2760d349af4fa0  MoganResearch-${version}.dmg
f4bcb8d4a9bec63b158c93fc0efbc083da95de84ddf7fb84ecf56675120458e5  mogan-research-${version}-ubuntu22.04.deb
6716064ec9f570aad3cbd8a84251d06cbbc7055f4875b323d2c7718a39ac9231  mogan-research-${version}-ubuntu24.04.deb
`);
</script>

<style module>
.button {
  color: var(--vp-c-white);
  background-color: var(--vp-c-brand);
  padding: 10px 20px;
  border: none;
  border-radius: 5px;
  font-weight: bold;
  cursor: pointer;
  transition: background-color 0.3s, transform 0.3s;
}

.button:hover {
  background-color: var(--vp-c-brand-light);
}

.button:active {
  background-color: var(--vp-c-brand-dark);
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
  background: var(--vp-c-white);
  color: var(--vp-c-text-1);
  padding: 20px;
  border-radius: 10px;
  text-align: left;
  max-width: 90%;
}

.closeButton {
  margin-top: 20px;
  padding: 5px 10px;
  font-size: 14px;
  color: var(--vp-c-white);
  background-color: var(--vp-c-brand);
  border: none;
  border-radius: 5px;
  cursor: pointer;
  transition: background-color 0.2s;
}

.closeButton:hover {
  background-color: var(--vp-c-brand-light);
}

.closeButton:active {
  background-color: var(--vp-c-brand-dark);
}

.dark .button {
  background-color: var(--vp-c-brand-dark);
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);
}

.dark .modalContent {
  background-color: var(--vp-c-bg-1); /* 假设存在背景颜色变量，可按需调整 */
  color: var(--vp-c-text-1);
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.7);
}

  .hoverContent {
    color: black;
  }
  
  .smallButton, .closeButton {
    color: white;
  }
  
</style>