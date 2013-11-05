photoWidth = 15
photoHeight = 20

unknownColor = '#D19D9D'
availableColor = '#9DD1B5'


zoom = d3.behavior.zoom()

adjustZoom = ->
  d3.select('#viewport')
    .attr('transform', "translate(#{d3.event.translate}) scale(#{d3.event.scale})");

machineGroups = [ 'pixel', 'texel', 'matrix', 'visual', 'corona', 'edge', 'unsgnd' ]

createMachine = (d, i) ->
  g = d3.select this
  x = d.pos.x - photoWidth / 2
  y = d.pos.y - photoHeight / 2

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

updateMachines = ->
  d3.json '/labstate', (err, ls) ->
    if ls['UNAVAILABLE']
      # message
    else
      stateData = for hostname, state of ls
        hostname: hostname
        state: state

      d3.select('#computers')
        .selectAll('g')
        .data(stateData, (d) -> d.hostname)
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
              .attr('xlink:href', 'img/' + d.state.photo)
              .transition()
              .delay(500)
              .style('opacity', 1)
              .each 'end', -> statusRect.attr('visibility', 'hidden')
            title.text "#{d.hostname} - #{d.state.fullName} (#{d.state.username})"

d3.xml 'labmap.svg', 'image/svg+xml', (xml) ->
  svg = d3.select('#map').select -> @appendChild xml.documentElement

  svg
    .call(zoom.on 'zoom', adjustZoom)

  desiredSize = window.innerWidth * 0.8

  xShift = (window.innerWidth - desiredSize) / 2
  yShift = (window.innerHeight - desiredSize) / 2

  zoom.size [desiredSize, desiredSize]
  zoom.translate [xShift, yShift]

  d3.json 'layout.json', (err, layout) ->
    machines = []

    for spline,descs of layout
      spline = d3.select('#' + spline)[0][0]
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
