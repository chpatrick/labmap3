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
sidebarOpen = false
changingFilter = false
lastFilter = null

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
    .attr('class', 'user-photo')
    .attr('id', (d) -> d.hostname)
    .attr('x', (d) -> d.pos.x - photoWidth / 2)
    .attr('y', (d) -> d.pos.y - photoHeight / 2)
    .attr('width', photoWidth)
    .attr('height', photoHeight)
    .attr('clip-path', 'url(#photo-clip-path)')
    .style('opacity', 0)
    .on 'click', (d) ->
      # find the parent's datum for clones
      datum = d || d3.select(@correspondingElement).datum()
      setTextFilter datum.state.fullName

availableFilter = (d, i) -> d.state == 'AVAILABLE'

textFilter = (filterString) ->
  words = filterString.toLowerCase().split(' ')
  (d, i) ->
    return false unless d.state.fullName?

    lowerCaseName = d.state.fullName.toLowerCase()
    for w in words
      return false if lowerCaseName.indexOf(w) == -1

    return true

updateTextFilter = ->
  return if changingFilter

  filterText = d3.select('#filter-box').property('value')

  changingFilter = true
  d3.select('#filter-available').property('checked', false)

  currentFilter = if filterText == "" then null else textFilter filterText
  updateFilter()
  changingFilter = false

setTextFilter = (filterText) ->
  if lastFilter == filterText
    lastFilter = null
    d3.select('#filter-box').property('value', '')
  else
    lastFilter = filterText
    d3.select('#filter-box').property('value', filterText)
  updateTextFilter()

updateHoists = ->
  matchingMachines = []
  if currentFilter?
    matchingMachines = (m for m in currentState when currentFilter m)

  hoists = d3.select('#hoisted')
    .selectAll('use')
    .data(matchingMachines, (d) -> d.hostname)

  hoists.enter()
    .append('use')
    .attr('xlink:href', (d) -> '#' + d.hostname)

  hoists.exit().remove()

updateUserList = (userList, userEntries) ->
  elements = userList.selectAll('li')
    .data(userEntries, (d) -> d.username)

  elements.enter()
    .append('li')
    .text((d) -> d.fullName)
    .on 'click', (d) -> setTextFilter d.fullName

  elements.exit()
    .remove()

updateFilter = ->
  overlayOpacity = if currentFilter? then 0.6 else 0

  d3.select('#shade')
    .transition()
    .attr('opacity', overlayOpacity)
    .each('start', ->
      if currentFilter?
        d3.select('#overlay').attr('visibility', 'visible')
        updateHoists())
    .each('end', ->
      if !currentFilter?
        d3.select('#overlay').attr('visibility', 'hidden')
        updateHoists())

  selectedUserEntries = []
  unselectedUserEntries = []

  for entry in currentState
    if entry.state.username
      if !currentFilter? or currentFilter entry
        selectedUserEntries.push entry.state
      else
        unselectedUserEntries.push entry.state

  updateUserList d3.select('#selected-user-list'), selectedUserEntries
  updateUserList d3.select('#unselected-user-list'), unselectedUserEntries

updateMachine = (d, i) ->
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
      .style('opacity', 1)
    photo.transition().style('opacity', 0)
    title.text "#{d.hostname} - #{d.state.toLowerCase()}"
  else
    photoUrl = 'img/' + (d.state.photo || 'anon.jpg')
    # Work around weird Chrome photo jumping.
    if photo.attr('xlink:href') != photoUrl
      photo.attr('xlink:href', photoUrl)  
    photo
      .transition()
      .delay(500)
      .style('opacity', if d.state.lockTime? then 0.5 else 1)
      .each 'end', -> statusRect.attr('visibility', 'hidden')
    if d.state.lockTime?
      statusRect
        .transition()
        .delay(500)
        .style('opacity', 0)
    title.text "#{d.hostname} - #{d.state.fullName} (#{d.state.username})"

updateMachines = ->
  d3.json '/labstate', (err, ls) ->
    if !ls?
      # TODO: message
    else if ls['UNAVAILABLE']
      # TODO: message
    else
      userEntries = []
      currentState = []

      for hostname, state of ls
        if state.username
          userEntries.push state

        currentState.push
          hostname: hostname
          state: state

      machines = d3.select('#computers')
        .selectAll('g')
        .data(currentState, (d) -> d.hostname)

      machines
        .each updateMachine
      machines
        .exit()
        .each (d, i) -> updateMachine.call this, { hostname: d.hostname, state: 'UNKNOWN' }, i

    updateFilter()

d3.xml 'labmap.svg', 'image/svg+xml', (xml) ->
  svg = d3.select('#map').select -> @appendChild xml.documentElement

  svg.call(zoom.on 'zoom', adjustZoom)

  desiredSize = window.innerHeight * 0.9
  scale = desiredSize / 750

  xShift = (window.innerWidth - desiredSize) / 2 * 0.95
  yShift = (window.innerHeight - desiredSize) / 2 * 1.3

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
  
    # setInterval updateMachines, 1000
    updateMachines()

    d3.select('#filter-available').on 'change', ->
      return if changingFilter

      changingFilter = true

      filterText = d3.select('#filter-box').property('value', '')

      if d3.select(this).property('checked')
        currentFilter = availableFilter
      else
        currentFilter = null
      updateFilter()
      changingFilter = false

    d3.select('#filter-box')
      .on('keyup', updateTextFilter)
      .on('change', updateTextFilter)

    d3.select('#sidebar-toggle').on 'click', ->
      d3.select('#sidebar')
        .transition()
        .style('right', if sidebarOpen then '-175px' else '0px')
        .each 'end', -> sidebarOpen = !sidebarOpen
