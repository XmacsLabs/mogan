export default {
    title: 'Mogan STEM Suite',
    outDir: "../website",
    locales: {
        "/": {
            lang: 'en-US',
            title: 'Mogan STEM Suite',
            description: 'libre STEM suite for exploring science and technology',
        },
        "/zh/": {
            lang: 'zh-CN',
            title: '墨干理工套件',
            description: '用于探索科学与技术的理工套件',
        },
    },

    lastUpdated: true,
    ignoreDeadLinks: false,
    head: [],

    themeConfig: {
        siteTitle: 'Mogan STEM Suite',
        locales: {
            '/': {
                selectLanguageName: 'English',
            },
            '/zh/': {
                selectLanguageName: '简体中文',
            },
        },
        nav: nav(),
        sidebar: {
            '/guide': sidebarGuide(),
            '/zh/guide': sidebarGuideZh(),
        },

        footer: {
            message: 'Let us enjoy exploring science and technology!',
            copyright: 'Copyright © 2022-2023 contributors of Mogan STEM Suite'
        },
        search: {
            provider: 'local',
        }
    }
}

function nav() {
    return [
        {
            text: 'Guide',
            link: '/guide/what-is-mogan',
            activeMatch: '/guide/'
        },
        {
            text: 'Forum',
            link: 'http://forum.texmacs.cn/'
        },
        {
            text: 'Language',
            items: [
                { text: '简体中文', link: '/zh/guide/what-is-mogan' },
                { text: 'English', link: '/guide/what-is-mogan' },
            ]
        },
        {
            text: 'Code (Gitee/Github)',
            items: [
                { text: 'Codeberg', link: 'https://codeberg.org/XmacsLabs/mogan' },
                { text: 'Gitee', link: 'https://gitee.com/XmacsLabs/mogan' },
                { text: 'Github', link: 'https://github.com/XmacsLabs/mogan' },
            ]
        },
    ]
}

function sidebarGuide() {
    return [
        {
            text: 'Introduction',
            items: [
                { text: 'What is Mogan?', link: '/guide/what-is-mogan' },
                { text: 'Major Features', link: '/guide/Feature' },
            ]
        },
        {
            text: 'Getting Started',
            items: [
                {
                    text: 'Install',
                    link: '/guide/Install'
                },
                {
                    text: 'Mogan v.s. TeXmacs',
                    link: '/guide/Mogan_versus_TeXmacs'
                },
            ]
        },
        {
            text: 'Community',
            items: [
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
                    text: 'Ghostscript Binary plugin',
                    link: '/guide/plugin_binary_gs.md'
                },
                {
                    text: 'Inkscape Binary plugin',
                    link: '/guide/plugin_binary_inkscape.md'
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
                    text: 'Maxima Session plugin',
                    link: '/guide/plugin_session_maxima.md'
                },
                {
                    text: 'Octave Session plugin',
                    link: '/guide/plugin_session_octave.md'
                },
                {
                    text: 'PDF Data plugin',
                    link: '/guide/plugin_data_pdf.md'
                },
            ],
        },
        {
            text: 'ChangeLog',
            collapsed: true,
            items: [
                {
                    text: 'v1.2.5 LTS (2024/02/27)',
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
                    link: '/guide/ChangeLog_v1.1.6.md'
                },
                {
                    text: 'v1.1.5 (2023/08/11)',
                    link: '/guide/ChangeLog_v1.1.5.md'
                },
                {
                    text: 'v1.1.4 (2023/07/31)',
                    link: '/guide/ChangeLog_v1.1.4.md'
                },
             ]
        },
    ]
}

function sidebarGuideZh() {
    return [
        {
            text: '简介',
            items: [
                { text: '墨干', link: '/zh/guide/what-is-mogan' },
                { text: '主要功能', link: '/zh/guide/Feature' },
            ]
        },
        {
            text: '如何入门',
            items: [
                {
                    text: '如何安装？',
                    link: '/zh/guide/Install'
                },
                {
                    text: '墨干 v.s. TeXmacs',
                    link: '/zh/guide/Mogan_versus_TeXmacs'
                },
            ]
        },
        {
            text: '社区',
            items: [
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
                    text: 'Ghostscript二进制插件',
                    link: '/zh/guide/plugin_binary_gs.md'
                },
                {
                    text: 'Inkscape二进制插件',
                    link: '/zh/guide/plugin_binary_inkscape.md'
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
                    text: 'Maxima会话插件',
                    link: '/zh/guide/plugin_session_maxima.md'
                },
                {
                    text: 'Octave会话插件',
                    link: '/zh/guide/plugin_session_octave.md'
                },
                {
                    text: 'PDF数据插件',
                    link: '/zh/guide/plugin_data_pdf.md'
                },
            ],
        },
        {
            text: '变更记录',
            collapsed: true,
            items: [
                {
                    text: 'v1.2.5 LTS (2024/02/27)',
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
                    link: '/zh/guide/ChangeLog_v1.1.6.md'
                },
                {
                    text: 'v1.1.5 (2023/08/11)',
                    link: '/zh/guide/ChangeLog_v1.1.5.md'
                },
                {
                    text: 'v1.1.4 (2023/07/31)',
                    link: '/zh/guide/ChangeLog_v1.1.4.md'
                },
                {
                    text: 'v1.1.3 (2023/06/05)',
                    link: '/zh/guide/ChangeLog_v1.1.3.md'
                },
                {
                    text: 'v1.1.2 (2023/04/09)',
                    link: '/zh/guide/ChangeLog_v1.1.2.md'
                },
                {
                    text: 'v1.1.1 (2022/10/31)',
                    link: '/zh/guide/ChangeLog_v1.1.1.md'
                },
                {
                    text: 'v1.1.0 (2022/08/31)',
                    link: '/zh/guide/ChangeLog_v1.1.0.md'
                },
                {
                    text: 'v1.0.4 (2022/05/28)',
                    link: '/zh/guide/ChangeLog_v1.0.4.md'
                },
                {
                    text: 'v1.0.3 (2022/05/15)',
                    link: '/zh/guide/ChangeLog_v1.0.3.md'
                },
                {
                    text: 'v1.0.2 (2022/05/04)',
                    link: '/zh/guide/ChangeLog_v1.0.2.md'
                },
                {
                    text: 'v1.0.1 (2022/03/26)',
                    link: '/zh/guide/ChangeLog_v1.0.1.md'
                },
                {
                    text: 'v1.0.0 (2022/01/31)',
                    link: '/zh/guide/ChangeLog_v1.0.0.md'
                },
            ]
        },
    ]
}
