photoWidth = 15
photoHeight = 20

unknownColor = '#D19D9D'
availableColor = '#9DD1B5'


zoom = d3.behavior.zoom()

adjustZoom = ->
  transform = "translate(#{d3.event.translate}) scale(#{d3.event.scale})"

  d3.select('#viewport').attr('transform', transform)
  d3.select('#hoisted').attr('transform', transform)

currentState = null
currentFilter = null

createMachine = (d, i) ->
  g = d3.select this
  x = d.pos.x - photoWidth / 2
  y = d.pos.y - photoHeight / 2

  g.attr 'id', d.hostname

  g.append('title').text d.hostname

  g.append('rect')
    .attr('x', x)
    .attr('y', y)
    .attr('width', photoWidth)
    .attr('height', photoHeight)
    .attr('clip-path', 'url(#photo-clip-path)')
    .attr('fill', unknownColor)
    #.attr('fill', colorbrewer.Pastel2[7][machineGroups.indexOf(d.group)])

  g.append('image')
    .attr('id', (d) -> d.hostname)
    .attr('x', (d) -> d.pos.x - photoWidth / 2)
    .attr('y', (d) -> d.pos.y - photoHeight / 2)
    .attr('width', photoWidth)
    .attr('height', photoHeight)
    .attr('clip-path', 'url(#photo-clip-path)')
    .style('opacity', 0)

availableFilter = (d, i) -> d.state == 'AVAILABLE'

updateFilter = ->
  matchingMachines = []

  overlayOpacity = if currentFilter? then 0.5 else 0

  d3.select('#overlay')
    .transition()
    .attr('opacity', overlayOpacity)

  if currentFilter?
    matchingMachines = (m for m in currentState when m.state is 'AVAILABLE')

  hoists = d3.select('#hoisted')
    .selectAll('use')
    .data(matchingMachines, (d) -> d.hostname)

  hoists.enter()
    .append('use')
    .attr('xlink:href', (d) -> '#' + d.hostname)

  hoists.exit().remove()

updateMachines = ->
  d3.json '/labstate', (err, ls) ->
    if ls['UNAVAILABLE']
      # message
    else
      currentState = for hostname, state of ls
        hostname: hostname
        state: state

      d3.select('#computers')
        .selectAll('g')
        .data(currentState, (d) -> d.hostname)
        .each (d, i) ->
          g = d3.select this
          statusRect = g.select 'rect'
          photo = g.select 'image'
          title = g.select 'title'
          if d.state is 'AVAILABLE' or d.state is 'UNKNOWN'
            color = if d.state is 'AVAILABLE' then availableColor else unknownColor
            statusRect
              .attr('visibility', 'visible')
              .transition()
              .attr('fill', color)
            photo.transition().style('opacity', 0)
            title.text "#{d.hostname} - #{d.state.toLowerCase()}"
          else
            photo
              .attr('xlink:href', 'img/' + (d.state.photo || 'anon.jpg'))
              .transition()
              .delay(500)
              .style('opacity', 1)
              .each 'end', -> statusRect.attr('visibility', 'hidden')
            title.text "#{d.hostname} - #{d.state.fullName} (#{d.state.username})"

    updateFilter()

d3.xml 'labmap.svg', 'image/svg+xml', (xml) ->
  svg = d3.select('#map').select -> @appendChild xml.documentElement

  svg.call(zoom.on 'zoom', adjustZoom)

  desiredSize = window.innerHeight * 0.8
  scale = desiredSize / 750

  xShift = (window.innerWidth - desiredSize) / 2 * 0.95
  yShift = (window.innerHeight - desiredSize) / 2

  zoom.scale scale
  zoom.translate [xShift, yShift]
  zoom.event svg

  d3.json 'layout.json', (err, layout) ->
    machines = []

    for spline,descs of layout
      spline = d3.select('#' + spline).node()
      splineLen = spline.getTotalLength()

      splineMachines = []      
      for desc in descs
        for i in [desc['from'] .. desc['to']]
          num = i + ''
          num = '0' + num if num.length < 2
          splineMachines.push
            group: desc['group']
            hostname: desc['group'] + num

      for machine, i in splineMachines
        dist = if splineMachines.length is 1 then 0 else splineLen * i / (splineMachines.length - 1)
        machine.pos = spline.getPointAtLength dist

      machines = machines.concat splineMachines

    d3.select('#computers')
      .selectAll('g')
      .data(machines, (d) -> d.hostname)
      .enter()
      .append('g')
      .each createMachine
  
    setInterval updateMachines, 1000

    d3.select('#filter-available').on 'change', ->
      if d3.select(this).property('checked')
        currentFilter = (d, i) -> d.state is 'AVAILABLE'
      else
        currentFilter = null
      updateFilter()
