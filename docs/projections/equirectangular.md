---
layout: projection
filename: equirectangular
title: Equirectangular
permalink: /projections/equirectangular/
math_blurb: Stereographic projections are invertible except at the point of projection, which is undefined.
threejs_code: /threejs/equirectangular.js
---
{%- comment -%}
{% assign projection = null %}
{% for item in site.data.projections %}
    {% if item.filename == page.filename %}
        {% assign projection = item %}
        {% break %}
    {% endif %}
{% endfor %}

## Background
<br>
## Mathematics
Stereographic projections are invertible except at the point of projection, which is undefined.

## Three.js Example

{% include threejs_template.html %}

{% if projection.threejs.href %}
<h3> Threejs Code </h3>
{% highlight javascript %}
{% include {{projection.threejs.href}} %}
{% endhighlight %}
{% endif %}

<h2> Code Snippets </h2>
<h3> Basic Mathematics Code </h3>
{{projection.general_code.notes}}
{% highlight javascript %}
{% include {{projection.threejs.href}} %}
{% endhighlight %}

### Pixel-Based Mathematics Code
{{projection.pixel_code.notes}}
{% highlight javascript %}
{% include {{projection.pixel_code.href}} %}
{% endhighlight %}

### Three.js Example Code
{{projection.threejs.notes}}
{% highlight javascript %}
{% include {{projection.threejs.href}} %}
{% endhighlight %}

{%- endcomment -%}
