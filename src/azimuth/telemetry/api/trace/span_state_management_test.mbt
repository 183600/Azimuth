// Trace API Span状态管理测试用例
// 测试Span的生命周期、状态转换和事件处理

test "span_lifecycle_state_transitions" {
  // 测试Span生命周期状态转换
  
  // 模拟Span状态
  enum SpanState {
    Created
    Started
    InProgress
    Finished
    Error
  }
  
  // 模拟Span
  struct TestSpan {
    name : String
    state : SpanState
    start_time : Int64?
    end_time : Int64?
    status : StatusCode
    events : Array[SpanEvent]
  }
  
  // 创建Span
  let span = TestSpan::{
    name: "test-span",
    state: Created,
    start_time: None,
    end_time: None,
    status: Unset,
    events: []
  }
  
  // 状态转换函数
  fn transition_state(span : TestSpan, new_state : SpanState, current_time : Int64) -> TestSpan {
    match (span.state, new_state) {
      (Created, Started) => TestSpan::{
        ..span, state: new_state, start_time: Some(current_time)
      }
      (Started, InProgress) => TestSpan::{ ..span, state: new_state }
      (InProgress, Finished) => TestSpan::{
        ..span, state: new_state, end_time: Some(current_time), status: Ok
      }
      (InProgress, Error) => TestSpan::{
        ..span, state: new_state, end_time: Some(current_time), status: Error
      }
      _ => span  // 无效转换，保持原状态
    }
  }
  
  // 测试状态转换
  let start_time = 1640995200000L
  let end_time = 1640995210000L
  
  let started_span = transition_state(span, Started, start_time)
  assert_eq(started_span.state, Started)
  match started_span.start_time {
    Some(time) => assert_eq(time, start_time)
    None => @test.fail("Test failed")
  }
  
  let in_progress_span = transition_state(started_span, InProgress, start_time + 1000L)
  assert_eq(in_progress_span.state, InProgress)
  
  let finished_span = transition_state(in_progress_span, Finished, end_time)
  assert_eq(finished_span.state, Finished)
  assert_eq(finished_span.status, Ok)
  match finished_span.end_time {
    Some(time) => assert_eq(time, end_time)
    None => @test.fail("Test failed")
  }
  
  // 测试错误状态转换
  let error_span = transition_state(in_progress_span, Error, end_time)
  assert_eq(error_span.state, Error)
  assert_eq(error_span.status, Error)
  
  // 测试无效状态转换
  let invalid_span = transition_state(finished_span, Started, end_time + 1000L)
  assert_eq(invalid_span.state, Finished)  // 应该保持原状态
}

test "span_status_management" {
  // 测试Span状态码管理
  
  // 模拟Span
  struct TestSpan {
    name : String
    status : StatusCode
    status_description : String?
  }
  
  // 创建Span
  let span = TestSpan::{
    name: "test-span",
    status: Unset,
    status_description: None
  }
  
  // 状态更新函数
  fn set_status(span : TestSpan, status : StatusCode, description? : String) -> TestSpan {
    TestSpan::{
      ..span, status: status, status_description: description
    }
  }
  
  // 测试状态设置
  let ok_span = set_status(span, Ok, None)
  assert_eq(ok_span.status, Ok)
  match ok_span.status_description {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  
  let error_span = set_status(span, Error, Some("Database connection failed"))
  assert_eq(error_span.status, Error)
  match error_span.status_description {
    Some(desc) => assert_eq(desc, "Database connection failed")
    None => @test.fail("Test failed")
  }
  
  // 测试状态优先级
  fn is_higher_priority(status1 : StatusCode, status2 : StatusCode) -> Bool {
    match (status1, status2) {
      (Error, Ok) => true
      (Error, Unset) => true
      (Ok, Unset) => true
      _ => false
    }
  }
  
  // 验证状态优先级
  assert_eq(is_higher_priority(Error, Ok), true)
  assert_eq(is_higher_priority(Error, Unset), true)
  assert_eq(is_higher_priority(Ok, Unset), true)
  assert_eq(is_higher_priority(Ok, Error), false)
  assert_eq(is_higher_priority(Unset, Ok), false)
  assert_eq(is_higher_priority(Unset, Error), false)
  
  // 测试状态合并逻辑
  fn merge_status(current : StatusCode, new_status : StatusCode) -> StatusCode {
    if is_higher_priority(new_status, current) {
      new_status
    } else {
      current
    }
  }
  
  // 验证状态合并
  assert_eq(merge_status(Unset, Ok), Ok)
  assert_eq(merge_status(Unset, Error), Error)
  assert_eq(merge_status(Ok, Error), Error)
  assert_eq(merge_status(Ok, Unset), Ok)
  assert_eq(merge_status(Error, Ok), Error)
  assert_eq(merge_status(Error, Unset), Error)
}

test "span_event_management" {
  // 测试Span事件管理
  
  // 模拟Span事件
  struct SpanEvent {
    name : String
    timestamp : Int64
    attributes : Array[(String, AttributeValue)]
  }
  
  // 模拟Span
  struct TestSpan {
    name : String
    events : Array[SpanEvent]
  }
  
  // 创建Span
  let span = TestSpan::{
    name: "test-span",
    events: []
  }
  
  // 添加事件函数
  fn add_event(span : TestSpan, name : String, timestamp : Int64, attributes : Array[(String, AttributeValue)]) -> TestSpan {
    let event = SpanEvent::{ name, timestamp, attributes }
    let new_events = []
    let mut i = 0
    while i < span.events.length() {
      new_events.push(span.events[i])
      i = i + 1
    }
    new_events.push(event)
    TestSpan::{ ..span, events: new_events }
  }
  
  // 添加测试事件
  let base_time = 1640995200000L
  let span_with_events = add_event(
    add_event(
      span,
      "database.query",
      base_time + 1000L,
      [("query", AttributeValue::string("SELECT * FROM users"))]
    ),
    "cache.hit",
    base_time + 2000L,
    [("key", AttributeValue::string("user:123"))]
  )
  
  // 验证事件数量
  assert_eq(span_with_events.events.length(), 2)
  
  // 验证事件内容
  assert_eq(span_with_events.events[0].name, "database.query")
  assert_eq(span_with_events.events[0].timestamp, base_time + 1000L)
  assert_eq(span_with_events.events[0].attributes.length(), 1)
  assert_eq(span_with_events.events[0].attributes[0].0, "query")
  
  assert_eq(span_with_events.events[1].name, "cache.hit")
  assert_eq(span_with_events.events[1].timestamp, base_time + 2000L)
  assert_eq(span_with_events.events[1].attributes.length(), 1)
  assert_eq(span_with_events.events[1].attributes[0].0, "key")
  
  // 测试事件排序
  fn are_events_sorted(events : Array[SpanEvent]) -> Bool {
    let mut i = 1
    while i < events.length() {
      if events[i-1].timestamp > events[i].timestamp {
        return false
      }
      i = i + 1
    }
    true
  }
  
  // 验证事件按时间排序
  assert_eq(are_events_sorted(span_with_events.events), true)
  
  // 测试事件过滤
  fn filter_events_by_name(events : Array[SpanEvent], name : String) -> Array[SpanEvent] {
    let filtered = []
    let mut i = 0
    while i < events.length() {
      if events[i].name == name {
        filtered.push(events[i])
      }
      i = i + 1
    }
    filtered
  }
  
  let db_events = filter_events_by_name(span_with_events.events, "database.query")
  assert_eq(db_events.length(), 1)
  assert_eq(db_events[0].name, "database.query")
  
  let cache_events = filter_events_by_name(span_with_events.events, "cache.hit")
  assert_eq(cache_events.length(), 1)
  assert_eq(cache_events[0].name, "cache.hit")
}

test "span_attribute_management" {
  // 测试Span属性管理
  
  // 模拟Span
  struct TestSpan {
    name : String
    attributes : Array[(String, AttributeValue)]
  }
  
  // 创建Span
  let span = TestSpan::{
    name: "test-span",
    attributes: []
  }
  
  // 设置属性函数
  fn set_attribute(span : TestSpan, key : String, value : AttributeValue) -> TestSpan {
    let new_attributes = []
    let mut found = false
    let mut i = 0
    
    // 复制现有属性，替换匹配的键
    while i < span.attributes.length() {
      let (k, v) = span.attributes[i]
      if k == key {
        new_attributes.push((key, value))
        found = true
      } else {
        new_attributes.push((k, v))
      }
      i = i + 1
    }
    
    // 如果没有找到，添加新属性
    if not found {
      new_attributes.push((key, value))
    }
    
    TestSpan::{ ..span, attributes: new_attributes }
  }
  
  // 添加属性
  let span_with_attrs = set_attribute(
    set_attribute(
      set_attribute(span, "http.method", AttributeValue::string("GET")),
      "http.status_code", AttributeValue::int(200L)
    ),
    "user.id", AttributeValue::string("12345")
  )
  
  // 验证属性数量
  assert_eq(span_with_attrs.attributes.length(), 3)
  
  // 验证属性内容
  let mut found_method = false
  let mut found_status = false
  let mut found_user = false
  
  let mut i = 0
  while i < span_with_attrs.attributes.length() {
    let (key, value) = span_with_attrs.attributes[i]
    match key {
      "http.method" => {
        found_method = true
        match value {
          StringValue(method) => assert_eq(method, "GET")
          _ => @test.fail("Test failed")
        }
      }
      "http.status_code" => {
        found_status = true
        match value {
          IntValue(status) => assert_eq(status, 200L)
          _ => @test.fail("Test failed")
        }
      }
      "user.id" => {
        found_user = true
        match value {
          StringValue(user_id) => assert_eq(user_id, "12345")
          _ => @test.fail("Test failed")
        }
      }
      _ => @test.fail("Unexpected attribute key: " + key)
    }
    i = i + 1
  }
  
  assert_eq(found_method, true)
  assert_eq(found_status, true)
  assert_eq(found_user, true)
  
  // 测试属性更新
  let updated_span = set_attribute(span_with_attrs, "http.status_code", AttributeValue::int(404L))
  
  // 验证属性已更新
  let mut found_updated = false
  let mut j = 0
  while j < updated_span.attributes.length() {
    let (key, value) = updated_span.attributes[j]
    if key == "http.status_code" {
      found_updated = true
      match value {
        IntValue(status) => assert_eq(status, 404L)
        _ => @test.fail("Test failed")
      }
    }
    j = j + 1
  }
  
  assert_eq(found_updated, true)
  assert_eq(updated_span.attributes.length(), 3)  // 数量不变
}

test "span_parent_child_relationships" {
  // 测试Span父子关系管理
  
  // 模拟Span上下文
  struct SpanContext {
    trace_id : Array[Byte]
    span_id : Array[Byte]
  }
  
  // 模拟Span
  struct TestSpan {
    name : String
    context : SpanContext
    parent_span_id : Array[Byte]?
    children : Array[Array[Byte]]
  }
  
  // 创建根Span
  let root_context = SpanContext::{
    trace_id: [0_byte, 1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte, 9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte],
    span_id: [16_byte, 17_byte, 18_byte, 19_byte, 20_byte, 21_byte, 22_byte, 23_byte]
  }
  
  let root_span = TestSpan::{
    name: "root-span",
    context: root_context,
    parent_span_id: None,
    children: []
  }
  
  // 创建子Span函数
  fn create_child_span(parent : TestSpan, name : String, span_id : Array[Byte]) -> TestSpan {
    let child_context = SpanContext::{
      trace_id: parent.context.trace_id,
      span_id: span_id
    }
    
    TestSpan::{
      name,
      context: child_context,
      parent_span_id: Some(parent.context.span_id),
      children: []
    }
  }
  
  // 添加子Span到父Span函数
  fn add_child_span(parent : TestSpan, child_span_id : Array[Byte]) -> TestSpan {
    let new_children = []
    let mut i = 0
    while i < parent.children.length() {
      new_children.push(parent.children[i])
      i = i + 1
    }
    new_children.push(child_span_id)
    TestSpan::{ ..parent, children: new_children }
  }
  
  // 创建子Span
  let child1_id = [24_byte, 25_byte, 26_byte, 27_byte, 28_byte, 29_byte, 30_byte, 31_byte]
  let child1 = create_child_span(root_span, "child-span-1", child1_id)
  
  let child2_id = [32_byte, 33_byte, 34_byte, 35_byte, 36_byte, 37_byte, 38_byte, 39_byte]
  let child2 = create_child_span(root_span, "child-span-2", child2_id)
  
  // 验证子Span的父关系
  match child1.parent_span_id {
    Some(parent_id) => {
      let mut matches = true
      let mut i = 0
      while i < parent_id.length() {
        if parent_id[i] != root_span.context.span_id[i] {
          matches = false
        }
        i = i + 1
      }
      assert_eq(matches, true)
    }
    None => @test.fail("Test failed")
  }
  
  match child2.parent_span_id {
    Some(parent_id) => {
      let mut matches = true
      let mut i = 0
      while i < parent_id.length() {
        if parent_id[i] != root_span.context.span_id[i] {
          matches = false
        }
        i = i + 1
      }
      assert_eq(matches, true)
    }
    None => @test.fail("Test failed")
  }
  
  // 验证子Span的trace_id与父Span相同
  let mut trace_id_matches = true
  let mut i = 0
  while i < child1.context.trace_id.length() {
    if child1.context.trace_id[i] != root_span.context.trace_id[i] {
      trace_id_matches = false
    }
    i = i + 1
  }
  assert_eq(trace_id_matches, true)
  
  // 更新父Span的子列表
  let root_with_children = add_child_span(
    add_child_span(root_span, child1.context.span_id),
    child2.context.span_id
  )
  
  // 验证父Span的子列表
  assert_eq(root_with_children.children.length(), 2)
  
  // 验证子Span ID在父Span的子列表中
  let mut found_child1 = false
  let mut found_child2 = false
  let mut i = 0
  while i < root_with_children.children.length() {
    let child_id = root_with_children.children[i]
    
    let mut matches_child1 = true
    let mut matches_child2 = true
    let mut j = 0
    
    while j < child_id.length() {
      if child_id[j] != child1.context.span_id[j] {
        matches_child1 = false
      }
      if child_id[j] != child2.context.span_id[j] {
        matches_child2 = false
      }
      j = j + 1
    }
    
    if matches_child1 {
      found_child1 = true
    }
    if matches_child2 {
      found_child2 = true
    }
    
    i = i + 1
  }
  
  assert_eq(found_child1, true)
  assert_eq(found_child2, true)
  
  // 创建孙Span
  let grandchild_id = [40_byte, 41_byte, 42_byte, 43_byte, 44_byte, 45_byte, 46_byte, 47_byte]
  let grandchild = create_child_span(child1, "grandchild-span", grandchild_id)
  
  // 验证孙Span的父关系
  match grandchild.parent_span_id {
    Some(parent_id) => {
      let mut matches = true
      let mut i = 0
      while i < parent_id.length() {
        if parent_id[i] != child1.context.span_id[i] {
          matches = false
        }
        i = i + 1
      }
      assert_eq(matches, true)
    }
    None => @test.fail("Test failed")
  }
}

test "span_timing_and_duration" {
  // 测试Span时间计算和持续时间
  
  // 模拟Span
  struct TestSpan {
    name : String
    start_time_unix_nanos : Int64
    end_time_unix_nanos : Int64?
  }
  
  // 创建Span函数
  fn create_span(name : String, start_time : Int64) -> TestSpan {
    TestSpan::{
      name,
      start_time_unix_nanos: start_time,
      end_time_unix_nanos: None
    }
  }
  
  // 结束Span函数
  fn end_span(span : TestSpan, end_time : Int64) -> TestSpan {
    TestSpan::{ ..span, end_time_unix_nanos: Some(end_time) }
  }
  
  // 计算持续时间函数
  fn duration(span : TestSpan) -> Int64? {
    match span.end_time_unix_nanos {
      Some(end_time) => Some(end_time - span.start_time_unix_nanos)
      None => None
    }
  }
  
  // 测试Span时间计算
  let start_time = 1640995200000000000L  // 纳秒
  let end_time = 1640995201000000000L   // 1秒后
  
  let span = create_span("test-span", start_time)
  assert_eq(span.start_time_unix_nanos, start_time)
  match span.end_time_unix_nanos {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  
  let finished_span = end_span(span, end_time)
  match finished_span.end_time_unix_nanos {
    Some(time) => assert_eq(time, end_time)
    None => @test.fail("Test failed")
  }
  
  // 计算持续时间
  match duration(finished_span) {
    Some(dur) => assert_eq(dur, 1000000000L)  // 1秒 = 10^9纳秒
    None => @test.fail("Test failed")
  }
  
  // 测试未结束Span的持续时间
  match duration(span) {
    None => assert_eq(true, true)
    Some(_) => @test.fail("Test failed")
  }
  
  // 测试时间转换函数
  fn nanos_to_milliseconds(nanos : Int64) -> Double {
    @double.from_int(nanos) / 1000000.0
  }
  
  fn nanos_to_seconds(nanos : Int64) -> Double {
    @double.from_int(nanos) / 1000000000.0
  }
  
  // 验证时间转换
  match duration(finished_span) {
    Some(dur) => {
      let duration_ms = nanos_to_milliseconds(dur)
      let duration_s = nanos_to_seconds(dur)
      assert_eq(duration_ms, 1000.0)
      assert_eq(duration_s, 1.0)
    }
    None => @test.fail("Test failed")
  }
  
  // 测试多个Span的时间比较
  let span1 = end_span(create_span("span1", start_time), start_time + 500000000L)
  let span2 = end_span(create_span("span2", start_time + 100000000L), start_time + 800000000L)
  let span3 = end_span(create_span("span3", start_time + 200000000L), start_time + 1200000000L)
  
  match duration(span1) {
    Some(dur1) => {
      match duration(span2) {
        Some(dur2) => {
          match duration(span3) {
            Some(dur3) => {
              assert_eq(dur1, 500000000L)  // 0.5秒
              assert_eq(dur2, 700000000L)  // 0.7秒
              assert_eq(dur3, 1000000000L) // 1.0秒
              
              // 验证持续时间比较
              assert_eq(dur1 < dur2, true)
              assert_eq(dur2 < dur3, true)
              assert_eq(dur1 < dur3, true)
            }
            None => @test.fail("Test failed")
          }
        }
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}