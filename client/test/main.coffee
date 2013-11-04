photoWidth = 15
photoHeight = 20

zoom = d3.behavior.zoom()

adjustZoom = ->
    d3.select('#viewport').attr('transform', "translate(#{d3.event.translate}) scale(#{d3.event.scale})");

d3.xml 'labmap.svg', 'image/svg+xml', (xml) ->
  svg = d3.select('#map').select -> @appendChild xml.documentElement

  svg
    .call(zoom.on 'zoom', adjustZoom)

  d3.json 'layout.json', (err, layout) ->
    machines = []

    for spline,descs of layout
      spline = d3.select('#' + spline)[0][0]
      splineLen = spline.getTotalLength()

      splineMachines = []      
      for desc in descs
        for i in [desc['from'] .. desc['to']]
          splineMachines.push
            hostname: desc['group'] + i

      for machine, i in splineMachines
        dist = if splineMachines.length is 1 then 0 else splineLen * i / (splineMachines.length - 1)
        machine.pos = spline.getPointAtLength dist

      machines = machines.concat splineMachines

    d3.select('#computers')
      .selectAll('image')
      .data(machines)
      .enter()
      .append('image')
      .attr('id', (d) -> d.hostname)
      .attr('x', (d) -> d.pos.x - photoWidth / 2)
      .attr('y', (d) -> d.pos.y - photoHeight / 2)
      .attr('width', photoWidth)
      .attr('height', photoHeight)
      .attr('clip-path', 'url(#photo-clip-path)')
      .attr('xlink:href', 'jr1610.jpg')
