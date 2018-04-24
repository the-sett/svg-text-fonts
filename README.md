# svg-text-fonts

svg-text-fonts supports converting strings to text suing OpenType fonts, and rendering
to SVG paths.

One propblem with SVG is that it is not possible to get the size of a piece of text
prior to rendering it, which can make it difficult to draw boxzes neatly around text.
This library allows  converting text into SVG paths so that their bounding box can be
accurately known ahead of rendering.

Text can also be rendered as SVG paths instead of by the browser, which can help with
geometric accuracy, and will allow fonts to animate smotthly without jittering caused by
browser text rendering using hinting to make text more legible. The text will be slightly
rougher this way, but this will not be so visible whilst text is being animated.

You can switch between path and broweser rendiring easily, as the there are 2 rendering
functions with a common type. This makes it possible to switch to path rendered text during
animation and back to browser rendered text when static, to improve legibility and ensure
text always looks crisp.
