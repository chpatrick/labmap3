photoWidth = 15
photoHeight = 20

unknownColor = '#D19D9D'
availableColor = '#9DD1B5'

lockMeanLog = 4.01311
lockSdLog = 1.46509

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

  g.append('image')
    .attr('class', 'user-photo')
    .attr('id', (d) -> d.hostname)
    .attr('x', (d) -> d.pos.x - photoWidth / 2)
    .attr('y', (d) -> d.pos.y - photoHeight / 2)
    .attr('width', photoWidth)
    .attr('height', photoHeight)
    .attr('clip-path', 'url(#photo-clip-path)')
    .style('opacity', 0)
    .style('display', 'none')
    .on 'click', (d) -> setTextFilter d.state.fullName if d?

availableFilter = (d, i) -> d.state == 'AVAILABLE'

textFilter = (filterString) ->
  words = []
  groups = []
  for w in filterString.toLowerCase().split(' ')
    if w.substr(0, 6) == 'group:'
      groups.push w.substr(6)
    else
      words.push w
  (d, i) ->
    return false unless d.state.fullName?

    lowerCaseName = d.state.fullName.toLowerCase()
    for w in words
      return false if lowerCaseName.indexOf(w) == -1
    for g in groups
      return false unless d.state.groupSet[g]

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
  toggleSidebar true
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
    .on 'click', (d) -> setTextFilter d.state.fullName if d.state.fullName

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
        d3.select('#overlay').style('display', '')
        updateHoists())
    .each('end', ->
      if !currentFilter?
        d3.select('#overlay').style('display', 'none')
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
      .style('display', '')
      .transition()
      .attr('fill', color)
      .style('opacity', 1)
    photo
      .style('display', 'none')
      .transition().style('opacity', 0)
    title.text "#{d.hostname} - #{d.state.toLowerCase()}"
  else
    photoUrl = 'img/' + (d.state.photo || 'anon.jpg')
    # Work around weird Chrome photo jumping.
    if photo.attr('xlink:href') != photoUrl
      photo.attr('xlink:href', photoUrl)  
    photo
      .style('display', '')
      .transition()
      .delay(500)
      .style('opacity', if d.state.lockProb? then 0.9 - d.state.lockProb * 0.6 else 1)
      .each 'end', -> statusRect.style('display', 'none')
    if d.state.lockTime?
      statusRect
        .transition()
        .delay(500)
        .style('opacity', 0)
    titleText = "#{d.hostname} - #{d.state.fullName} (#{d.state.username})"
    if d.state.lockProb
      titleText += " - #{Math.floor(d.state.lockProb * 100)}% chance of AFK"
    title.text titleText

erf = (x) ->
  # constants
  a1 = 0.254829592
  a2 = -0.284496736
  a3 = 1.421413741
  a4 = -1.453152027
  a5 = 1.061405429
  p = 0.3275911
  
  # Save the sign of x
  sign = 1
  sign = -1  if x < 0
  x = Math.abs(x)
  
  # A&S formula 7.1.26
  t = 1.0 / (1.0 + p * x)
  y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.exp(-x * x)
  sign * y

erfc = (x) -> 1 - erf(x)

logNormalCdf = (x, meanlog, sdlog) -> 1 / 2.0 * erfc(-(Math.log(x) - meanlog)/(sdlog * Math.sqrt(2)))

logNormalCcdf = (x, meanlog, sdlog) -> 1 - logNormalCdf(x, meanlog, sdlog)

lockedProbability = (lockTime, lockDuration) ->
  logNormalCcdf(lockTime, lockMeanLog, lockSdLog) / logNormalCcdf(lockDuration, lockMeanLog, lockSdLog)

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
        if state.lockTime
          lockTime = (new Date().getTime() - new Date(state.lockTime).getTime()) / 1000 / 60
          state.lockProb = lockedProbability lockTime, state.lockDuration

        if state.username
          userEntries.push state
          state.groupSet = {}
          state.groupSet[group] = true for group in state.groups

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

toggleSidebar = (open) ->
  if sidebarOpen != open
    d3.select('#sidebar')
      .transition()
      .style('right', if sidebarOpen then '-175px' else '0px')
      .each 'end', -> sidebarOpen = !sidebarOpen

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
  
    setInterval updateMachines, 1000

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
      toggleSidebar !sidebarOpen
