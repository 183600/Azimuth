// 多传播器组合测试
// 测试遥测系统中多个传播器的组合使用和交互

import "azimuth/azimuth"

pub test "多传播器基础组合测试" {
  // 创建不同类型的传播器
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let custom_propagator = azimuth::CustomPropagator::new("custom-header")
  
  // 创建复合传播器
  let composite_propagator = azimuth::CompositePropagator::new([
    trace_propagator,
    baggage_propagator,
    custom_propagator
  ])
  
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 添加上下文值
  let key1 = azimuth::ContextKey::new("test.key1")
  let key2 = azimuth::ContextKey::new("test.key2")
  let ctx_with_values = azimuth::Context::with_value(
    azimuth::Context::with_value(ctx, key1, "value1"),
    key2, 
    "value2"
  )
  
  // 注入上下文
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_values, carrier)
  
  // 验证所有传播器都参与了注入
  let trace_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  let custom_header = azimuth::TextMapCarrier::get(carrier, "custom-header")
  
  assert_true(trace_header.is_some())
  assert_true(baggage_header.is_some())
  assert_true(custom_header.is_some())
  
  // 提取上下文
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取的上下文值
  let extracted_value1 = azimuth::Context::get(extracted_ctx, key1)
  let extracted_value2 = azimuth::Context::get(extracted_ctx, key2)
  
  assert_eq(extracted_value1, Some("value1"))
  assert_eq(extracted_value2, Some("value2"))
}

pub test "传播器优先级和顺序测试" {
  // 创建多个自定义传播器
  let high_priority_propagator = azimuth::CustomPropagator::with_priority("high-priority", 100)
  let medium_priority_propagator = azimuth::CustomPropagator::with_priority("medium-priority", 50)
  let low_priority_propagator = azimuth::CustomPropagator::with_priority("low-priority", 10)
  
  // 创建有序复合传播器
  let ordered_propagator = azimuth::OrderedCompositePropagator::new([
    low_priority_propagator,
    high_priority_propagator,
    medium_priority_propagator
  ])
  
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 注入上下文
  azimuth::OrderedCompositePropagator::inject(ordered_propagator, ctx, carrier)
  
  // 验证传播器按优先级顺序执行
  let headers = azimuth::TextMapCarrier::all_headers(carrier)
  assert_eq(headers.length(), 3)
  
  // 验证高优先级传播器的值不被覆盖
  let high_priority_value = azimuth::TextMapCarrier::get(carrier, "high-priority")
  let medium_priority_value = azimuth::TextMapCarrier::get(carrier, "medium-priority")
  let low_priority_value = azimuth::TextMapCarrier::get(carrier, "low-priority")
  
  assert_true(high_priority_value.is_some())
  assert_true(medium_priority_value.is_some())
  assert_true(low_priority_value.is_some())
  
  // 测试提取顺序
  let extracted_ctx = azimuth::OrderedCompositePropagator::extract(ordered_propagator, carrier)
  
  // 验证高优先级传播器的值被优先提取
  let priority_key = azimuth::ContextKey::new("priority.test")
  let priority_value = azimuth::Context::get(extracted_ctx, priority_key)
  
  // 高优先级传播器的值应该被保留
  assert_eq(priority_value, Some("high-priority-value"))
}

pub test "传播器冲突解决测试" {
  // 创建可能冲突的传播器
  let propagator1 = azimuth::CustomPropagator::with_header("shared-header", "propagator1-value")
  let propagator2 = azimuth::CustomPropagator::with_header("shared-header", "propagator2-value")
  let propagator3 = azimuth::CustomPropagator::with_header("shared-header", "propagator3-value")
  
  // 创建带冲突解决的复合传播器
  let conflict_resolving_propagator = azimuth::ConflictResolvingCompositePropagator::new([
    propagator1,
    propagator2,
    propagator3
  ], azimuth::ConflictStrategy::FirstWins)  // 第一个传播器获胜
  
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 注入上下文
  azimuth::ConflictResolvingCompositePropagator::inject(conflict_resolving_propagator, ctx, carrier)
  
  // 验证冲突解决策略
  let shared_value = azimuth::TextMapCarrier::get(carrier, "shared-header")
  assert_eq(shared_value, Some("propagator1-value"))  // 第一个传播器的值获胜
  
  // 测试不同冲突解决策略
  let last_wins_propagator = azimuth::ConflictResolvingCompositePropagator::new([
    propagator1,
    propagator2,
    propagator3
  ], azimuth::ConflictStrategy::LastWins)  // 最后一个传播器获胜
  
  let carrier2 = azimuth::TextMapCarrier::new()
  azimuth::ConflictResolvingCompositePropagator::inject(last_wins_propagator, ctx, carrier2)
  
  let last_wins_value = azimuth::TextMapCarrier::get(carrier2, "shared-header")
  assert_eq(last_wins_value, Some("propagator3-value"))  // 最后一个传播器的值获胜
  
  // 测试合并策略
  let merge_strategy_propagator = azimuth::ConflictResolvingCompositePropagator::new([
    propagator1,
    propagator2,
    propagator3
  ], azimuth::ConflictStrategy::Merge)  // 合并所有值
  
  let carrier3 = azimuth::TextMapCarrier::new()
  azimuth::ConflictResolvingCompositePropagator::inject(merge_strategy_propagator, ctx, carrier3)
  
  let merged_value = azimuth::TextMapCarrier::get(carrier3, "shared-header")
  assert_eq(merged_value, Some("propagator1-value,propagator2-value,propagator3-value"))
}

pub test "条件传播器测试" {
  // 创建条件传播器
  let always_true_condition = fn(ctx: Context) { true }
  let always_false_condition = fn(ctx: Context) { false }
  let custom_condition = fn(ctx: Context) { 
    // 基于上下文值决定是否传播
    let key = azimuth::ContextKey::new("should.propagate")
    let value = azimuth::Context::get(ctx, key)
    value == Some("true")
  }
  
  let conditional_propagator1 = azimuth::ConditionalPropagator::new(
    azimuth::CustomPropagator::new("conditional1"),
    always_true_condition
  )
  
  let conditional_propagator2 = azimuth::ConditionalPropagator::new(
    azimuth::CustomPropagator::new("conditional2"),
    always_false_condition
  )
  
  let conditional_propagator3 = azimuth::ConditionalPropagator::new(
    azimuth::CustomPropagator::new("conditional3"),
    custom_condition
  )
  
  // 创建条件复合传播器
  let conditional_composite = azimuth::CompositePropagator::new([
    conditional_propagator1,
    conditional_propagator2,
    conditional_propagator3
  ])
  
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 测试条件为true的情况
  let ctx_with_true = azimuth::Context::with_value(
    ctx, 
    azimuth::ContextKey::new("should.propagate"), 
    "true"
  )
  
  azimuth::CompositePropagator::inject(conditional_composite, ctx_with_true, carrier)
  
  // 验证条件传播器
  let conditional1_header = azimuth::TextMapCarrier::get(carrier, "conditional1")
  let conditional2_header = azimuth::TextMapCarrier::get(carrier, "conditional2")
  let conditional3_header = azimuth::TextMapCarrier::get(carrier, "conditional3")
  
  assert_true(conditional1_header.is_some())  // always true，应该传播
  assert_true(conditional2_header.is_none())  // always false，不应该传播
  assert_true(conditional3_header.is_some())  // custom condition为true，应该传播
  
  // 测试条件为false的情况
  let carrier2 = azimuth::TextMapCarrier::new()
  let ctx_with_false = azimuth::Context::with_value(
    ctx, 
    azimuth::ContextKey::new("should.propagate"), 
    "false"
  )
  
  azimuth::CompositePropagator::inject(conditional_composite, ctx_with_false, carrier2)
  
  let conditional1_header2 = azimuth::TextMapCarrier::get(carrier2, "conditional1")
  let conditional2_header2 = azimuth::TextMapCarrier::get(carrier2, "conditional2")
  let conditional3_header2 = azimuth::TextMapCarrier::get(carrier2, "conditional3")
  
  assert_true(conditional1_header2.is_some())   // always true，应该传播
  assert_true(conditional2_header2.is_none())   // always false，不应该传播
  assert_true(conditional3_header2.is_none())   // custom condition为false，不应该传播
}

pub test "传播器链和管道测试" {
  // 创建传播器链
  let first_propagator = azimuth::CustomPropagator::new("first")
  let second_propagator = azimuth::CustomPropagator::new("second")
  let third_propagator = azimuth::CustomPropagator::new("third")
  
  // 创建传播器管道
  let pipeline = azimuth::PropagatorPipeline::new()
  azimuth::PropagatorPipeline::add_stage(pipeline, first_propagator)
  azimuth::PropagatorPipeline::add_stage(pipeline, second_propagator)
  azimuth::PropagatorPipeline::add_stage(pipeline, third_propagator)
  
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 通过管道注入
  azimuth::PropagatorPipeline::inject(pipeline, ctx, carrier)
  
  // 验证管道执行
  let first_header = azimuth::TextMapCarrier::get(carrier, "first")
  let second_header = azimuth::TextMapCarrier::get(carrier, "second")
  let third_header = azimuth::TextMapCarrier::get(carrier, "third")
  
  assert_true(first_header.is_some())
  assert_true(second_header.is_some())
  assert_true(third_header.is_some())
  
  // 测试管道中间件
  let logging_middleware = azimuth::PropagatorMiddleware::new(fn(next, ctx, carrier) {
    // 记录传播操作
    azimuth::Logger::info("Propagator middleware executing")
    next(ctx, carrier)
  })
  
  let pipeline_with_middleware = azimuth::PropagatorPipeline::new()
  azimuth::PropagatorPipeline::add_stage(pipeline_with_middleware, first_propagator)
  azimuth::PropagatorPipeline::add_middleware(pipeline_with_middleware, logging_middleware)
  azimuth::PropagatorPipeline::add_stage(pipeline_with_middleware, second_propagator)
  
  let carrier2 = azimuth::TextMapCarrier::new()
  azimuth::PropagatorPipeline::inject(pipeline_with_middleware, ctx, carrier2)
  
  // 验证中间件执行
  let first_header2 = azimuth::TextMapCarrier::get(carrier2, "first")
  let second_header2 = azimuth::TextMapCarrier::get(carrier2, "second")
  
  assert_true(first_header2.is_some())
  assert_true(second_header2.is_some())
}

pub test "传播器缓存和性能优化测试" {
  // 创建缓存传播器
  let cached_propagator = azimuth::CachedPropagator::new(
    azimuth::CustomPropagator::new("cached"),
    100  // 缓存大小
  )
  
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 第一次注入（应该执行实际传播）
  let start_time1 = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  azimuth::CachedPropagator::inject(cached_propagator, ctx, carrier)
  let end_time1 = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration1 = end_time1 - start_time1
  
  // 第二次注入相同上下文（应该使用缓存）
  let carrier2 = azimuth::TextMapCarrier::new()
  let start_time2 = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  azimuth::CachedPropagator::inject(cached_propagator, ctx, carrier2)
  let end_time2 = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let duration2 = end_time2 - start_time2
  
  // 验证缓存效果
  assert_true(duration2 < duration1)  // 缓存应该更快
  
  // 验证缓存统计
  let cache_stats = azimuth::CachedPropagator::cache_statistics(cached_propagator)
  assert_eq(azimuth::CacheStats::hits(cache_stats), 1)
  assert_eq(azimuth::CacheStats::misses(cache_stats), 1)
  
  // 测试批量传播器
  let batch_propagator = azimuth::BatchPropagator::new([
    azimuth::CustomPropagator::new("batch1"),
    azimuth::CustomPropagator::new("batch2"),
    azimuth::CustomPropagator::new("batch3")
  ])
  
  let carriers = []
  for i in 0..10 {
    let batch_carrier = azimuth::TextMapCarrier::new()
    carriers.push(batch_carrier)
  }
  
  // 批量注入
  let batch_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  azimuth::BatchPropagator::batch_inject(batch_propagator, ctx, carriers)
  let batch_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let batch_duration = batch_end - batch_start
  
  // 验证批量传播
  for carrier in carriers {
    let batch1_header = azimuth::TextMapCarrier::get(carrier, "batch1")
    let batch2_header = azimuth::TextMapCarrier::get(carrier, "batch2")
    let batch3_header = azimuth::TextMapCarrier::get(carrier, "batch3")
    
    assert_true(batch1_header.is_some())
    assert_true(batch2_header.is_some())
    assert_true(batch3_header.is_some())
  }
  
  // 验证批量传播效率
  assert_true(batch_duration < duration1 * 10)  // 批量应该比单独执行10次更快
}

pub test "传播器错误处理和容错测试" {
  // 创建容错传播器
  let failing_propagator = azimuth::FailingPropagator::new("failing", 0.5)  // 50%失败率
  let normal_propagator = azimuth::CustomPropagator::new("normal")
  
  let fault_tolerant_composite = azimuth::FaultTolerantCompositePropagator::new([
    failing_propagator,
    normal_propagator
  ])
  
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  // 多次尝试注入
  let successful_injections = 0
  let failed_injections = 0
  
  for i in 0..10 {
    let test_carrier = azimuth::TextMapCarrier::new()
    let injection_result = azimuth::FaultTolerantCompositePropagator::inject(
      fault_tolerant_composite, 
      ctx, 
      test_carrier
    )
    
    if (azimuth::InjectionResult::is_success(injection_result)) {
      successful_injections = successful_injections + 1
      
      // 至少应该有正常传播器的头
      let normal_header = azimuth::TextMapCarrier::get(test_carrier, "normal")
      assert_true(normal_header.is_some())
    } else {
      failed_injections = failed_injections + 1
    }
  }
  
  // 验证容错性
  assert_true(successful_injections > 5)  // 至少50%成功
  assert_true(failed_injections < 5)     // 最多50%失败
  
  // 测试传播器恢复
  let recovering_propagator = azimuth::RecoveringPropagator::new(
    azimuth::CustomPropagator::new("recovering"),
    3  // 最大重试次数
  )
  
  let recovery_carrier = azimuth::TextMapCarrier::new()
  let recovery_result = azimuth::RecoveringPropagator::inject_with_retry(
    recovering_propagator, 
    ctx, 
    recovery_carrier
  )
  
  // 验证恢复机制
  assert_true(azimuth::InjectionResult::is_success(recovery_result))
  
  let recovering_header = azimuth::TextMapCarrier::get(recovery_carrier, "recovering")
  assert_true(recovering_header.is_some())
  
  // 验证重试统计
  let retry_stats = azimuth::RecoveringPropagator::retry_statistics(recovering_propagator)
  assert_true(azimuth::RetryStats::total_attempts(retry_stats) >= 1)
}