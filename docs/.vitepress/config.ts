import mathjax3 from 'markdown-it-mathjax3'

export default {
    title: 'Mogan STEM Suite',
    outDir: "../website",
    themeConfig: {
        search: {
            provider: 'local'
        }
    },
    locales: {
        root: {
            label: 'English',
            lang: 'en',
            title: 'Mogan STEM Suite',
            description: 'libre STEM suite for exploring science and technology',
            themeConfig: {
                siteTitle: 'Mogan',
                logo: {
                src: '/mogan-logo.png',
                alt: '墨干Logo'
                },
                nav: nav(),
                sidebar: {
                    '/guide': sidebarGuide(),
                },
                footer: {
                    message: 'Enjoy exploring science and technology!',
                    copyright: 'Copyright © 2022-2025 Mogan Contributors'
                },
            }
        },
        zh: {
            label: '简体中文',
            lang: 'zh',
            title: '墨干理工套件',
            description: '用于探索科学与技术的自由的理工套件',
            themeConfig: {
                siteTitle: '墨干',
                logo: {
                src: '/mogan-logo.png',
                alt: 'Mogan Logo'
                },
                nav: navZh(),
                sidebar: {
                    '/zh/guide': sidebarGuideZh(),
                },
                footer: {
                    message: '享受探索科学与技术的乐趣！',
                    copyright: '版权所有 © 2022-2025 墨干贡献者'
                },
            }
        },
    },
    lastUpdated: true,
    ignoreDeadLinks: false,
    head: [
        ['link', { rel: 'icon', href: '/favicon.png' }],
    ],
    markdown: {
        config(md) {
            md.use(mathjax3);
        },
    }
}

function nav() {
    return [
        {
            text: '',
            link: '/',
            activeMatch: '/',
        },
        {
            text: 'Tutorial on Mogan🔥',
            link: '/zh/guide/Tutorial',
            activeMatch: '/ospp/'
        },
        {
            text: 'Guide',
            link: '/guide/what-is-mogan',
            activeMatch: '/guide/'
        },
        {
            text: 'Forum',
            link: 'http://forum.texmacs.cn'
        },
        {
            text: 'Code',
            link: '/guide/SourceCode'
        },
    ]
}

function navZh() {
    return [
        {
            text: '',
            link: '/zh/',
            activeMatch: '/zh/',
        },
        {
            text: '零基础墨干🔥',
            link: '/zh/guide/Tutorial',
            activeMatch: '/ospp/'
        },
        {
            text: '大模型集成',
            link: '/zh/guide/plugin_llm',
            activeMatch: '/ospp/'
        },
        {
            text: 'AI时代的Latex零基础入门',
            link: '/zh/guide/AI_Latex_introduction',
        },
        {
            text: '指南',
            link: '/zh/guide/what-is-mogan',
            activeMatch: '/guide/'
        },
        {
            text: '下载',
            link: '/zh/guide/Install',
            activeMatch: '/guide/Install'
        },
        {
            text: '论坛',
            link: 'http://forum.texmacs.cn'
        },
        {
            text: '源代码',
            link: '/zh/guide/SourceCode'
        },
    ]
}

function sidebarGuide() {
    return [
        {
            text: 'Getting Started',
            items: [
                { text: 'What is Mogan?', link: '/guide/what-is-mogan' },
                {
                    text: 'Install',
                    link: '/guide/Install'
                },
                {
                    text: 'FAQ',
                    link: '/guide/FAQ'
                },
                { text: 'Screenshots', link: '/guide/Screenshots' },
                {
                    text: 'Mogan v.s. TeXmacs',
                    link: '/guide/Mogan_versus_TeXmacs'
                },
                {
                    text: 'DOCX Data Plugin',
                    link: '/guide/plugin_data_docx.md'
                },
                {
                    text: 'Maxima Plugin',
                    link: '/guide/plugin_maxima.md'
                },
                {
                    text: 'Python Plugin',
                    link: '/guide/plugin_python.md'
                },
                {
                    text: 'Goldfish Scheme',
                    link: '/guide/plugin_goldfish.md'
                },
            ]
        },
        {
            text: 'Community',
            items: [
                {
                    text: 'Summer of Code',
                    link: '/guide/SummerOfCode.md'
                },
                {
                    text: 'Contact us',
                    link: '/guide/Contact'
                },
            ]
        },
        {
            text: 'Plugin System',
            items: [
                {
                    text: 'Introduction to Plugins',
                    link: '/guide/plugins.md'
                },
                {
                    text: 'Binary Plugin',
                    link: '/guide/plugin_binary.md'
                },
            ],
        },
        {
            text: 'Builtin Plugins',
            collapsed: true,
            items: [
                {
                    text: 'Aspell Binary plugin',
                    link: '/guide/plugin_binary_aspell.md'
                },
                {
                    text: 'Conda Binary plugin',
                    link: '/guide/plugin_binary_conda.md'
                },
                {
                    text: 'Ghostscript Binary plugin',
                    link: '/guide/plugin_binary_gs.md'
                },
                {
                    text: 'Hunspell Binary plugin',
                    link: '/guide/plugin_binary_hunspell.md'
                },
                {
                    text: 'Inkscape Binary plugin',
                    link: '/guide/plugin_binary_inkscape.md'
                },
                {
                    text: 'Pandoc Binary plugin',
                    link: '/guide/plugin_binary_pandoc.md'
                },
                {
                    text: 'Python3 Binary plugin',
                    link: '/guide/plugin_binary_python3.md'
                },
                {
                    text: 'rsvg-convert Binary plugin',
                    link: '/guide/plugin_binary_rsvg_convert.md'
                },
                {
                    text: 'PDF Image plugin',
                    link: '/guide/plugin_image_pdf.md'
                },
                {
                    text: 'SVG Image plugin',
                    link: '/guide/plugin_image_svg.md'
                },
                {
                    text: 'PDF Data plugin',
                    link: '/guide/plugin_data_pdf.md'
                },
            ],
        },
        {
            text: "Getting Involved",
            collapsed: true,
            items: [
                {
                    text: 'Contribution',
                    link: '/guide/Contribution.md'
                },
                {
                    text: 'Develop on Linux',
                    link: '/guide/Develop_on_Linux.md'
                },
                {
                    text: 'Develop on macOS',
                    link: '/guide/Develop_on_macOS.md'
                },
                {
                    text: 'Develop on Windows',
                    link: '/guide/Develop_on_Windows.md'
                },
                {
                    text: 'How to test',
                    link: '/guide/Test.md'
                }
            ]
        },
        {
            text: 'Roadmap',
            collapsed: true,
            items: [
                {
                    text: 'Releases',
                    link: '/guide/Releases.md'
                },
                {
                    text: 'v1.2.9.x LTS',
                    link: '/guide/changelog/v1.2.9.md'
                },
                {
                    text: 'v1.2.8 (2024/08/08)',
                    link: '/guide/changelog/v1.2.8.md'
                },
                {
                    text: 'v1.2.6 (2024/05/21)',
                    link: '/guide/changelog/v1.2.6.md'
                },
                {
                    text: 'v1.2.5.x LTS',
                    link: '/guide/ChangeLog_v1.2.5.md'
                },
                {
                    text: 'v1.2.4 (2024/02/01)',
                    link: '/guide/ChangeLog_v1.2.4.md'
                },
                {
                    text: 'v1.2.3 (2024/01/30)',
                    link: '/guide/ChangeLog_v1.2.3.md'
                },
                {
                    text: 'v1.2.2 (2023/12/23)',
                    link: '/guide/ChangeLog_v1.2.2.md'
                },
                {
                    text: 'v1.2.1 (2023/12/21)',
                    link: '/guide/ChangeLog_v1.2.1.md'
                },
                {
                    text: 'v1.2.0 (2023/12/03)',
                    link: '/guide/ChangeLog_v1.2.0.md'
                },
                {
                    text: 'v1.1.6 (2023/09/29)',
                    link: '/guide/changelog/v1.1.6.md'
                },
                {
                    text: 'v1.1.5 (2023/08/11)',
                    link: '/guide/changelog/v1.1.5.md'
                },
                {
                    text: 'v1.1.4 (2023/07/31)',
                    link: '/guide/changelog/v1.1.4.md'
                },
            ]
        },
    ]
}

function sidebarGuideZh() {
    return [
        {
            text: '入门',
            items: [
                { text: '墨干', link: '/zh/guide/what-is-mogan' },
                {
                    text: '如何安装？',
                    link: '/zh/guide/Install'
                },
                {
                    text: '快捷键',
                    link: '/zh/guide/keyboard_shortcuts'
                },
                {
                    text: '常见问题',
                    link: '/zh/guide/FAQ'
                },
                { text: '截图', link: '/zh/guide/Screenshots' },
                {
                    text: '墨干 v.s. TeXmacs',
                    link: '/zh/guide/Mogan_versus_TeXmacs'
                },
                {
                    text: 'DOCX数据插件',
                    link: '/zh/guide/plugin_data_docx.md'
                },
                {
                    text: 'Maxima插件',
                    link: '/zh/guide/plugin_maxima.md'
                },
                {
                    text: 'Python插件',
                    link: '/zh/guide/plugin_python.md'
                },
                {
                    text: 'AI大模型插件',
                    link: '/zh/guide/plugin_llm.md'
                },
                {
                    text: '金鱼Scheme',
                    link: '/zh/guide/plugin_goldfish.md'
                },
            ]
        },
        {
            text: '社区',
            items: [
                {
                    text: '《零基础墨干》视频教程',
                    link: '/zh/guide/Tutorial'
                },
                {
                    text: "SICP公开课",
                    link: '/zh/guide/SICP'
                },
                {
                    text: 'AI时代的Latex零基础入门',
                    link: '/zh/guide/AI_Latex_introduction.md'
                },
                {
                    text: '联系我们',
                    link: '/zh/guide/Contact'
                },
            ]
        },
        {
            text: '插件体系',
            items: [
                {
                    text: '插件概述',
                    link: '/zh/guide/plugins.md'
                },
                {
                    text: '二进制插件',
                    link: '/zh/guide/plugin_binary.md'
                },
            ]
        },
        {
            text: "内置插件",
            collapsed: true,
            items: [
                {
                    text: 'Aspell二进制插件',
                    link: '/zh/guide/plugin_binary_aspell.md'
                },
                {
                    text: 'Conda二进制插件',
                    link: '/zh/guide/plugin_binary_conda.md'
                },
                {
                    text: 'Ghostscript二进制插件',
                    link: '/zh/guide/plugin_binary_gs.md'
                },
                {
                    text: 'Hunspell二进制插件',
                    link: '/zh/guide/plugin_binary_hunspell.md'
                },
                {
                    text: 'Inkscape二进制插件',
                    link: '/zh/guide/plugin_binary_inkscape.md'
                },
                {
                    text: 'Pandoc Binary plugin',
                    link: '/zh/guide/plugin_binary_pandoc.md'
                },
                {
                    text: 'Python3二进制插件',
                    link: '/zh/guide/plugin_binary_python3.md'
                },
                {
                    text: 'rsvg-convert二进制插件',
                    link: '/zh/guide/plugin_binary_rsvg_convert.md'
                },
                {
                    text: 'PDF图像插件',
                    link: '/zh/guide/plugin_image_pdf.md'
                },
                {
                    text: 'SVG图像插件',
                    link: '/zh/guide/plugin_image_svg.md'
                },
                {
                    text: 'PDF数据插件',
                    link: '/zh/guide/plugin_data_pdf.md'
                },
            ],
        },
        {
            text: "如何参与",
            collapsed: true,
            items: [
                {
                    text: '如何贡献',
                    link: '/zh/guide/Contribution.md'
                },
                {
                    text: 'Linux平台开发指南',
                    link: '/zh/guide/Develop_on_Linux.md'
                },
                {
                    text: 'macOS平台开发指南',
                    link: '/zh/guide/Develop_on_macOS.md'
                },
                {
                    text: 'Windows平台开发指南',
                    link: '/zh/guide/Develop_on_Windows.md'
                },
                {
                    text: '如何测试',
                    link: '/zh/guide/Test.md'
                }
            ]
        },
        {
            text: '路线图',
            collapsed: true,
            items: [
                {
                    text: '版本发布',
                    link: '/zh/guide/Releases.md'
                },
                {
                    text: 'v1.2.9.x LTS',
                    link: '/zh/guide/changelog/v1.2.9.md'
                },
                {
                    text: 'v1.2.8 (2024/08/08)',
                    link: '/zh/guide/changelog/v1.2.8.md'
                },
                {
                    text: 'v1.2.6 (2024/05/21)',
                    link: '/zh/guide/changelog/v1.2.6.md'
                },
                {
                    text: 'v1.2.5.x LTS',
                    link: '/zh/guide/ChangeLog_v1.2.5.md'
                },
                {
                    text: 'v1.2.4 (2024/02/01)',
                    link: '/zh/guide/ChangeLog_v1.2.4.md'
                },
                {
                    text: 'v1.2.3 (2024/01/30)',
                    link: '/zh/guide/ChangeLog_v1.2.3.md'
                },
                {
                    text: 'v1.2.2 (2023/12/23)',
                    link: '/zh/guide/ChangeLog_v1.2.2.md'
                },
                {
                    text: 'v1.2.1 (2023/12/21)',
                    link: '/zh/guide/ChangeLog_v1.2.1.md'
                },
                {
                    text: 'v1.2.0 (2023/12/03)',
                    link: '/zh/guide/ChangeLog_v1.2.0.md'
                },
                {
                    text: 'v1.1.6 (2023/09/29)',
                    link: '/zh/guide/changelog/v1.1.6.md'
                },
                {
                    text: 'v1.1.5 (2023/08/11)',
                    link: '/zh/guide/changelog/v1.1.5.md'
                },
                {
                    text: 'v1.1.4 (2023/07/31)',
                    link: '/zh/guide/changelog/v1.1.4.md'
                },
                {
                    text: 'v1.1.3 (2023/06/05)',
                    link: '/zh/guide/changelog/v1.1.3.md'
                },
                {
                    text: 'v1.1.2 (2023/04/09)',
                    link: '/zh/guide/changelog/v1.1.2.md'
                },
                {
                    text: 'v1.1.1 (2022/10/31)',
                    link: '/zh/guide/changelog/v1.1.1.md'
                },
                {
                    text: 'v1.1.0 (2022/08/31)',
                    link: '/zh/guide/changelog/v1.1.0.md'
                },
                {
                    text: 'v1.0.4 (2022/05/28)',
                    link: '/zh/guide/changelog/v1.0.4.md'
                },
                {
                    text: 'v1.0.3 (2022/05/15)',
                    link: '/zh/guide/changelog/v1.0.3.md'
                },
                {
                    text: 'v1.0.2 (2022/05/04)',
                    link: '/zh/guide/changelog/v1.0.2.md'
                },
                {
                    text: 'v1.0.1 (2022/03/26)',
                    link: '/zh/guide/changelog/v1.0.1.md'
                },
                {
                    text: 'v1.0.0 (2022/01/31)',
                    link: '/zh/guide/changelog/v1.0.0.md'
                },
            ]
        },
    ]
}