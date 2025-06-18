<template>
  <div class="custom-hero">
    <div class="floating-particles"></div>
    <!-- PC端布局 -->
    <div class="hero-container" v-if="!isMobile">
      <div class="hero-content text-content">
        <h1 class="title animate-fade-in">
          <span class="gradient-text">公式 · 代码 · 排版</span>
        </h1>
        <p class="subtitle animate-slide-up">百倍速公式编辑！</p>
        <p class="description animate-slide-up-delay">
          所见即所得，轻松达到
          <span v-html="katex.renderToString('\\LaTeX')"></span> 级排版效果！
        </p>
        <div class="feature-highlights">
          <div class="tag">
            <strong>商业版 Mogan</strong> |
            <span class="tag-desc"
              >使用<a
                href="https://liiistem.cn"
                target="_blank"
                rel="noopener noreferrer"
                class="no-underline"
                >Liii STEM</a
              >，拥抱AI写作</span
            >
          </div>
          <div class="tag">
            <strong>智能识别格式</strong> |
            <span class="tag-desc">自动识别并转换多种文档格式</span>
          </div>
          <div class="tag">
            <strong>使用魔法粘贴</strong> |
            <span class="tag-desc">一键粘贴公式表格代码</span>
          </div>
        </div>
      </div>
      <div class="hero-right-section">
        <div class="hero-content image-section">
          <div class="image-content">
            <img src="/mogan-logo.png" alt="Liii STEM Logo" class="hero-logo" />
          </div>
        </div>
        <div class="actions animate-fade-in">
          <a href="/zh/guide/what-is-mogan" class="cta-button primary">
            关于 Mogan
          </a>
          <a href="/zh/guide/Install" class="cta-button secondary"> 下载 </a>
        </div>
      </div>
    </div>
    <!-- 移动端布局 -->
    <div class="hero-container mobile" v-else>
      <div class="hero-content image-section">
        <div class="image-content">
          <img src="/mogan-logo.png" alt="Liii STEM Logo" class="hero-logo" />
        </div>
      </div>
      <div class="hero-content text-content">
        <h1 class="title animate-fade-in">
          <span class="gradient-text">公式 · 代码 · 排版</span>
        </h1>
        <p class="subtitle animate-slide-up">百倍速公式编辑！</p>
        <p class="description animate-slide-up-delay">
          所见即所得，轻松达到
          <span v-html="katex.renderToString('\\LaTeX')"></span> 级排版效果！
        </p>
      </div>
      <div class="actions animate-fade-in">
        <a href="/zh/guide/what-is-mogan" class="cta-button primary">
          关于 Mogan
        </a>
        <a href="/zh/guide/Install" class="cta-button secondary"> 下载 </a>
      </div>
    </div>
  </div>
</template>

<script setup>
import { useRouter } from "vitepress";
import katex from "katex";
import "katex/dist/katex.min.css";
import { ref, onMounted, onUnmounted } from "vue";

const router = useRouter();
const isMobile = ref(false);

const checkMobile = () => {
  isMobile.value = window.innerWidth <= 1000;
};

onMounted(() => {
  checkMobile();
  window.addEventListener("resize", checkMobile);
});

onUnmounted(() => {
  window.removeEventListener("resize", checkMobile);
});

const scrollToElement = (id) => {
  setTimeout(() => {
    const element = document.getElementById(id);
    if (element) {
      const elementRect = element.getBoundingClientRect();
      const absoluteElementTop =
        element.getBoundingClientRect().top + window.scrollY;
      const middle =
        absoluteElementTop - window.innerHeight / 2 + elementRect.height / 2;

      window.scrollTo({
        top: middle,
        behavior: "smooth",
      });
    }
  }, 0);
};

const navigate = (path) => {
  if (path.startsWith("#")) {
    scrollToElement(path.slice(1));
  } else {
    router.go(path);
  }
};
</script>

<style scoped>
/* ============================================
   基础布局
============================================ */
.custom-hero {
  position: relative;
  padding: 4rem 4rem;
  overflow: hidden;
  min-height: 600px;
  display: flex;
  align-items: center;
}

.hero-container {
  max-width: 1400px;
  margin: 0 auto;
  display: flex;
  align-items: flex-start;
  gap: 2rem;
  position: relative;
  z-index: 1;
  padding: 2rem 1rem;
}

/* ============================================
   左侧文本区域
============================================ */
.hero-content {
  flex: 1;
}

/* 文本内容相关样式 */
.text-content {
  flex: 3;
  padding: 2rem 2rem;
  margin: 0;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  min-width: 0;
  overflow: hidden;
}

/* 标题和描述文本样式 */
.title,
.subtitle,
.description {
  width: 100%;
  text-align: left;
  white-space: nowrap;
  margin: 0;
}

.title {
  font-size: clamp(2.8rem, 5vw, 4.5rem) !important;
  line-height: 1.1;
  margin-top: 0.3rem;
  margin-bottom: 1.5rem;
  font-weight: 800;
  letter-spacing: -0.5px;
}

.subtitle {
  font-size: clamp(1.7rem, 4vw, 2.7rem) !important;
  margin-bottom: 0.5rem;
  line-height: 1.2;
  font-weight: 600;
  color: var(--vp-c-text-1);
}

.description {
  font-size: clamp(1.1rem, 2vw, 1.7rem) !important;
  margin: 4rem 0 8.25rem;
  line-height: 1.5;
  color: var(--vp-c-text-2);
}

/* 修改特性标签样式 */
.feature-highlights {
  display: flex;
  flex-direction: column;
  gap: 1rem;
  margin: -7rem 0 3rem;
  width: 100%;
  color: var(--vp-c-text-2);
}

.feature-highlights .feature-tag {
  font-size: 0.95rem;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.feature-highlights .tag-icon {
  font-size: 1.1rem;
}

.feature-highlights .tag-desc {
  color: var(--vp-c-text-2);
  font-size: 0.9rem;
  margin-left: 0.5rem;
}

.no-underline {
  text-decoration: none;
}

/* ============================================
   右侧区域重构
============================================ */
.hero-right-section {
  flex: 1;
  display: flex;
  flex-direction: column;
  max-width: 450px;
  position: relative;
  align-items: flex-end;
}

.image-section {
  flex: none;
  padding: 1.4rem 1rem;
  margin-bottom: 2rem;
  width: 100%;
  margin-top: -15px;
}

/* Logo样式及效果 */
.image-content {
  width: 100%;
  display: flex;
  justify-content: flex-end;
}

.hero-logo {
  width: 400px;
  height: auto;
  filter: drop-shadow(0 10px 20px var(--vp-c-brand-dimm-2));
  transition: transform 0.3s ease;
  margin-left: auto;
}

:root.dark .hero-logo {
  filter: drop-shadow(0 10px 25px var(--vp-c-brand-dimm-1));
}

/* ============================================
   按钮样式
============================================ */
.actions {
  display: flex;
  flex-direction: row;
  gap: 1rem;
  width: auto;
  align-items: center;
  justify-content: flex-end;
  padding-right: 1rem;
}

@keyframes float {
  0%,
  100% {
    transform: translateY(0);
  }

  50% {
    transform: translateY(-20px);
  }
}

.gradient-text {
  background: linear-gradient(
    120deg,
    var(--vp-c-brand) 0%,
    var(--vp-c-brand-light) 50%,
    var(--vp-c-brand) 100%
  );
  background-size: 200% auto;
  animation: shine 8s linear infinite;
  -webkit-background-clip: text;
  background-clip: text;
  -webkit-text-fill-color: transparent;
}

.cta-button {
  padding: 0.8rem 1.6rem;
  border-radius: 24px;
  font-weight: 500;
  transition: all 0.3s ease;
  position: relative;
  overflow: hidden;
  text-decoration: none;
  width: 160px;
  text-align: center;
}

/* 修改按钮样式 */
.cta-button.primary {
  background: linear-gradient(
    120deg,
    var(--vp-c-brand) 0%,
    var(--vp-c-brand-light) 100%
  );
  color: white;
}

.cta-button.primary:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px var(--vp-c-brand-dimm-1);
}

.cta-button.secondary {
  border: 1px solid var(--vp-c-brand);
  color: var(--vp-c-brand);
}

.cta-button.secondary:hover {
  background: var(--vp-c-brand-dimm-1);
  transform: translateY(-2px);
}

.cta-button:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 8px var(--vp-c-brand-dimm-1);
}

/* ============================================
   动画效果
============================================ */
@keyframes fadeIn {
  from {
    opacity: 0;
  }

  to {
    opacity: 1;
  }
}

@keyframes slideUp {
  from {
    opacity: 0;
    transform: translateY(20px);
  }

  to {
    opacity: 1;
    transform: translateY(0);
  }
}

@keyframes shine {
  to {
    background-position: 200% center;
  }
}

@keyframes pulse {
  0% {
    transform: scale(1) rotate(0deg);
    opacity: 0.1;
  }

  100% {
    transform: scale(1.1) rotate(45deg);
    opacity: 0.15;
  }
}

@keyframes particleFloat {
  from {
    transform: translateY(0);
  }

  to {
    transform: translateY(-50px);
  }
}

.animate-fade-in {
  animation: fadeIn 1s ease-out;
}

.animate-slide-up {
  animation: slideUp 1s ease-out;
}

.animate-slide-up-delay {
  animation: slideUp 1s ease-out 0.3s backwards;
}

.floating-particles {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  pointer-events: none;
  background-image: radial-gradient(
    circle,
    var(--vp-c-brand) 1px,
    transparent 1px
  );
  background-size: 50px 50px;
  opacity: 0.1;
  animation: particleFloat 20s linear infinite;
}

/* ============================================
   响应式布局 (1000px - 1300px)
============================================ */
@media (max-width: 1300px) {
  .hero-container {
    gap: 1.5rem;
    padding: 1rem;
    margin: 0 auto;
    max-width: calc(100% - 2rem);
  }

  .hero-right-section {
    max-width: 420px;
  }

  .title {
    font-size: 3rem !important;
  }

  .subtitle {
    font-size: 2rem !important;
  }

  .description {
    font-size: 1.5rem !important;
    margin: 3.5rem 0 7rem;
  }

  .feature-highlights {
    margin: -4rem 0 2rem;
  }

  .feature-highlights .tag {
    font-size: 0.9rem;
  }

  .feature-highlights .tag-desc {
    font-size: 0.85rem;
  }

  .actions {
    margin: -1.5rem 0 0;
  }
}

@media (max-width: 1100px) {
  .hero-container {
    max-width: calc(100% - 1.5rem);
    padding: 0.73rem;
  }

  .subtitle {
    font-size: 1.6rem !important;
  }

  .hero-right-section {
    max-width: 380px;
  }
}

/* ============================================
   移动端适配 (1000px以下)
============================================ */
@media (max-width: 1000px) {
  .custom-hero {
    padding: 2rem 1rem;
    min-height: auto;
  }

  .hero-container.mobile {
    flex-direction: column;
    align-items: center;
    padding: 1rem;
    gap: 2rem;
  }

  .feature-highlights {
    display: none;
  }

  .image-section {
    width: 100%;
    margin: 0;
    padding: 0;
  }

  .image-content {
    justify-content: center;
    margin: 0;
  }

  .hero-logo {
    width: 180px;
    margin: 0 auto;
    filter: drop-shadow(0 8px 16px var(--vp-c-brand-dimm-1));
  }

  .text-content {
    width: 100%;
    max-width: 600px;
    margin: 2rem auto;
    padding: 0;
    align-items: center;
    text-align: center;
  }

  .title,
  .subtitle,
  .description {
    text-align: center;
    white-space: normal;
  }

  .title {
    font-size: 1.8rem !important;
    margin: 0 0 1rem;
  }

  .subtitle {
    font-size: 1.3rem !important;
    margin: 0 0 0.5rem;
  }

  .description {
    font-size: 0.9rem !important;
    margin: 1rem auto 2rem;
  }

  .actions {
    width: 100%;
    max-width: 320px;
    margin: 0 auto;
    justify-content: center;
    padding-right: 0;
  }

  .cta-button {
    width: 140px;
    padding: 0.6rem 1.2rem;
    font-size: 0.9rem;
  }

  .floating-particles {
    opacity: 0.07;
    background-size: 40px 40px;
  }
}
</style>
