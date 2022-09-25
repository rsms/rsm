---
layout: simple2
title: rsm virtual computer
---

<style>
.video {
  max-width: 900px;
  margin: 0px auto;
  margin-top:    var(--blockSpacingTop);
  margin-bottom: var(--blockSpacingBottom);
}
.video > div {
  position: relative;
  padding-bottom: 55%;
  height: 0px;
}
.video iframe {
  position: absolute;
  top: 0px;
  left: 0px;
  width: 100%;
  height: 100%;
}
</style>

# rsm

`rsm` is a virtual computer, a fun hobby project with an accompanying video diary.


[Project on GitHub](https://github.com/rsms/rsm)<br>
[Initial conversation on twitter](https://twitter.com/rsms/status/1492582847982440448)<br>
[Related tweets](https://twitter.com/search?q=from%3Arsms%20%22rsm%22&f=live)

[Try RSM online in the playground](play/)

Project goals:
1. learn, have fun -- simplicity
2. substrate, a thing to make other thing on
3. longevity -- I want to be able to run a (multimedia) program in 10+ years


<a name="part-1"></a>
## Episode 1

<!--video src="https://d.rsms.me/rsm/rsm-hack-project-part-1-220214-1920-nomusic.mp4" typ
e="video/mp4" controls="true" width="100%"></video-->

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/yQOHA6OBjAg?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>

Feb 14, 2022: Kicking things off. Motivations, goals, initial take on ISA design.
[MP4](https://d.rsms.me/rsm/rsm-hack-project-part-1-220214-1920-nomusic.mp4)
[transcript](ep1.html)


<a name="part-2"></a>
## Episode 2

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/ZOd1f7wc4jE?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>
Feb 15, 2022: Part two in which I mostly struggle with getting the sign bit "to stick" on instruction arguments. Also covers a simple code formatter.
[MP4](https://d.rsms.me/rsm/rsm-hack-project-part-2-220215-1920.mp4)
[transcript](ep2.html)


<a name="part-3"></a>
## Episode 3

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/0dZ5T7ZRLWw?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>
Feb 15, 2022: Signed integer hurdles from part 2 solved. Review changes, talk about and do some code organization.
[MP4](https://d.rsms.me/rsm/rsm-hack-project-part-3-220215-1920.mp4)
[transcript](ep3.html)


<a name="part-4"></a>
## Episode 4

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/9uPNoxshJfw?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>
Feb 16, 2022: Building the beginning of the VM evaluator and looking at three different strategies for instruction handler dispatch.
[MP4](https://d.rsms.me/rsm/rsm-hack-project-part-4-220216-1920.mp4)
[transcript](ep4.html)

_This was a live stream where I had some technical difficulties with crashing streaming software which also failed to store videos locally, so apologies for the quality — I had to download the low-quality live videos from youtube and cut them together. You can find the full live streams here in two segments: [segment 1](https://youtu.be/sDJGGwYlM1g), [segment 2](https://youtu.be/RRIPFCAYfP0)_


<a name="part-5"></a>
## Episode 5

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/dAdaLyBlPMA?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>
Feb 17, 2022: VM is alive! We look at how the factorial function is evaluated by the vm and talk about the future challenge of constant data addressing.
[MP4](https://d.rsms.me/rsm/rsm-hack-project-part-5-220217-1920.mp4)
[transcript](ep5.html)


<a name="part-6"></a>
## Episode 6

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/6IHnZXtoQ-Q?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>
Feb 18, 2022: Getting started with the assembler: parsing RSM assembly source text.
[MP4](https://d.rsms.me/rsm/rsm-part-6-220218-1920.mp4)
[transcript](ep6.html)


<a name="part-7"></a>
## Episode 7

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/A9KtyRzk40Q?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>
Feb 18, 2022: Summarizing the implementation of the assembly parser.
[MP4](https://d.rsms.me/rsm/rsm-part-7-220218.mp4)
[transcript](ep7.html)

I did a long live stream when writing the parser:

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/xzG4g5v51Fo?start=315&modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>


## Episode 8

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/1jqsUCyRzT8?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>
Feb 19, 2022: The assembly parser now fully parses the factorial example function! I discuss the latest syntax and how function bodies are now strictly block-based.
[MP4](https://d.rsms.me/rsm/rsm-part-8-220219.mp4)
[transcript](ep8.html)


## Episode 9

<div class="video"><div><iframe src="https://www.youtube-nocookie.com/embed/4RgK0CSri2I?modestbranding=1&color=white&rel=0&cc_load_policy=1" width="720" height="396" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div></div>
Feb 20, 2022: Wrap up the hack week and check out the now complete virtual machine! We can compile and run arbitrary programs, within the constraints of the current features of course.
[MP4](https://d.rsms.me/rsm/rsm-ep9-220220.mp4)
[transcript](ep9.html)


[Videos are also available as a YouTube playlist](https://www.youtube.com/playlist?list=PLafx3JqbS4DC_uU4JaMxpc_Tfa7qE40do).
