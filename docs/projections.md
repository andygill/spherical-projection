---
layout: page
title: Projections
permalink: /projections/
---

This is a list of all the projections and their links
{% assign order = site.data.projections | sort : "title" %}

{% for post in order %}
## [{{post.title}}]({{post.url}})
{% endfor %}
