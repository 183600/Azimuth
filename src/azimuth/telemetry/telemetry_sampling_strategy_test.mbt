// 遥测采样策略测试
// 测试各种采样策略在不同场景下的效果和性能

test "sampling_strategy_probability_based" {
  // 测试基于概率的采样策略
  
  let total_spans = 10000
  let sampling_probability = 0.1  // 10%采样率
  let base_trace_id = 0x1234567890abcdefL
  
  // 模拟基于概率的采样决策
  let sampling_decisions = []
  let mut i = 0
  
  while i < total_spans {
    let trace_id = base_trace_id + i.to_int64()
    
    // 简化的概率采样算法：基于trace_id的哈希值
    let hash_value = (trace_id * 2654435761L) % 100L
    let should_sample = hash_value < (sampling_probability * 100.0).to_int64()
    
    sampling_decisions.push((trace_id, should_sample))
    i = i + 1
  }
  
  // 验证采样决策数量
  assert_eq(sampling_decisions.length(), total_spans)
  
  // 计算实际采样率
  let sampled_count = sampling_decisions.filter(fn(decision) { decision.1 }).length()
  let actual_sampling_rate = sampled_count.to_double() / total_spans.to_double()
  
  // 验证采样率在预期范围内（允许5%的误差）
  let expected_lower = sampling_probability - 0.05
  let expected_upper = sampling_probability + 0.05
  assert_eq(actual_sampling_rate >= expected_lower && actual_sampling_rate <= expected_upper, true)
  
  // 验证采样分布均匀性
  let sampled_trace_ids = sampling_decisions.filter(fn(decision) { decision.1 })
                                           .map(fn(decision) { decision.0 })
  
  // 检查采样trace_id的分布
  let mut min_trace_id = sampled_trace_ids[0]
  let mut max_trace_id = sampled_trace_ids[0]
  let mut j = 1
  while j < sampled_trace_ids.length() {
    if sampled_trace_ids[j] < min_trace_id {
      min_trace_id = sampled_trace_ids[j]
    }
    if sampled_trace_ids[j] > max_trace_id {
      max_trace_id = sampled_trace_ids[j]
    }
    j = j + 1
  }
  
  // 验证采样范围覆盖了整个输入范围
  let range_coverage = (max_trace_id - min_trace_id).to_double() / (total_spans.to_int64()).to_double()
  assert_eq(range_coverage >= 0.8, true)  // 至少覆盖80%的范围
}

test "sampling_strategy_adaptive_rate" {
  // 测试自适应采样率策略
  
  let time_windows = 24  // 24个时间窗口
  let base_load = 100    // 基础负载
  let peak_load_multiplier = 5  // 峰值负载倍数
  let max_sampling_rate = 0.2   // 最大采样率20%
  let min_sampling_rate = 0.01  // 最小采样率1%
  let target_sampled_count = 200  // 目标采样数量
  
  // 模拟不同时间窗口的负载变化
  let window_loads = []
  let mut i = 0
  
  while i < time_windows {
    // 模拟日常负载模式：早晚高峰
    let load_multiplier = match i {
      0..5 => 0.3    // 深夜低负载
      6..8 => 2.0    // 早晨高峰
      9..11 => 1.2   // 上午正常
      12..13 => 1.5  // 午间小高峰
      14..17 => 1.0  // 下午正常
      18..20 => 3.0  // 晚间高峰
      _ => 0.5       // 夜间低负载
    }
    
    let window_load = (base_load.to_double() * load_multiplier).to_int()
    window_loads.push(window_load)
    i = i + 1
  }
  
  // 为每个窗口计算自适应采样率
  let adaptive_sampling_rates = []
  let mut j = 0
  
  while j < window_loads.length() {
    let current_load = window_loads[j]
    
    // 自适应采样率计算：基于负载调整采样率
    let adaptive_rate = if current_load <= target_sampled_count {
      max_sampling_rate
    } else {
      let calculated_rate = target_sampled_count.to_double() / current_load.to_double()
      if calculated_rate > max_sampling_rate {
        max_sampling_rate
      } else if calculated_rate < min_sampling_rate {
        min_sampling_rate
      } else {
        calculated_rate
      }
    }
    
    adaptive_sampling_rates.push(adaptive_rate)
    j = j + 1
  }
  
  // 验证采样率在合理范围内
  let mut k = 0
  while k < adaptive_sampling_rates.length() {
    let rate = adaptive_sampling_rates[k]
    assert_eq(rate >= min_sampling_rate && rate <= max_sampling_rate, true)
    k = k + 1
  }
  
  // 验证负载与采样率的反比关系
  let mut l = 0
  while l < adaptive_sampling_rates.length() - 1 {
    let current_load = window_loads[l]
    let next_load = window_loads[l + 1]
    let current_rate = adaptive_sampling_rates[l]
    let next_rate = adaptive_sampling_rates[l + 1]
    
    // 负载增加时，采样率应该降低或保持不变
    if next_load > current_load * 1.5 {
      assert_eq(next_rate <= current_rate * 1.1, true)
    }
    
    l = l + 1
  }
  
  // 模拟实际采样数量
  let actual_sampled_counts = []
  let mut m = 0
  while m < window_loads.length() {
    let load = window_loads[m]
    let rate = adaptive_sampling_rates[m]
    let sampled_count = (load.to_double() * rate).to_int()
    actual_sampled_counts.push(sampled_count)
    m = m + 1
  }
  
  // 验证实际采样数量接近目标
  let mut total_sampled = 0
  let mut n = 0
  while n < actual_sampled_counts.length() {
    total_sampled = total_sampled + actual_sampled_counts[n]
    n = n + 1
  }
  
  let average_sampled_per_window = total_sampled.to_double() / time_windows.to_double()
  assert_eq(average_sampled_per_window >= target_sampled_count.to_double() * 0.8, true)
  assert_eq(average_sampled_per_window <= target_sampled_count.to_double() * 1.2, true)
}

test "sampling_strategy_attribute_based" {
  // 测试基于属性的采样策略
  
  let total_requests = 5000
  let critical_services = ["payment", "auth", "user-profile"]
  let critical_operations = ["checkout", "login", "register"]
  let error_sampling_rate = 1.0     // 错误请求100%采样
  let critical_sampling_rate = 0.5  // 关键服务50%采样
  let normal_sampling_rate = 0.05   // 正常请求5%采样
  
  // 模拟带有不同属性的请求
  let requests_with_attributes = []
  let mut i = 0
  
  while i < total_requests {
    let service = ["api", "payment", "auth", "database", "cache"][i % 5]
    let operation = ["get", "post", "put", "delete", "checkout", "login"][i % 6]
    let status_code = [200, 200, 200, 404, 500, 200, 200, 503][i % 8]
    let user_tier = ["free", "premium", "enterprise"][i % 3]
    
    let request = {
      "service": service,
      "operation": operation,
      "status_code": status_code,
      "user_tier": user_tier,
      "request_id": i.to_string()
    }
    requests_with_attributes.push(request)
    i = i + 1
  }
  
  // 基于属性的采样决策
  let sampling_decisions = []
  let mut j = 0
  
  while j < requests_with_attributes.length() {
    let request = requests_with_attributes[j]
    let mut sampling_rate = normal_sampling_rate
    
    // 基于状态码调整采样率
    if request["status_code"] >= 400 {
      sampling_rate = error_sampling_rate
    }
    // 基于关键服务调整采样率
    else if critical_services.contains(request["service"]) {
      sampling_rate = critical_sampling_rate
    }
    // 基于关键操作调整采样率
    else if critical_operations.contains(request["operation"]) {
      sampling_rate = critical_sampling_rate
    }
    // 基于用户层级调整采样率
    else if request["user_tier"] == "enterprise" {
      sampling_rate = sampling_rate * 2.0  // 企业用户采样率翻倍
    }
    
    // 确保采样率不超过100%
    if sampling_rate > 1.0 {
      sampling_rate = 1.0
    }
    
    // 采样决策
    let hash_value = (j.to_int64() * 2654435761L) % 100L
    let should_sample = hash_value < (sampling_rate * 100.0).to_int64()
    
    sampling_decisions.push((request, should_sample, sampling_rate))
    j = j + 1
  }
  
  // 验证采样决策数量
  assert_eq(sampling_decisions.length(), total_requests)
  
  // 分析不同类别的采样效果
  let error_requests = sampling_decisions.filter(fn(decision) { decision.0["status_code"] >= 400 })
  let critical_service_requests = sampling_decisions.filter(fn(decision) { 
    critical_services.contains(decision.0["service"]) 
  })
  let normal_requests = sampling_decisions.filter(fn(decision) { 
    !critical_services.contains(decision.0["service"]) && decision.0["status_code"] < 400 
  })
  
  // 验证错误请求采样率
  let error_sampled_count = error_requests.filter(fn(decision) { decision.1 }).length()
  let error_sampling_rate_actual = if error_requests.length() > 0 {
    error_sampled_count.to_double() / error_requests.length().to_double()
  } else {
    0.0
  }
  assert_eq(error_sampling_rate_actual >= 0.9, true)  // 至少90%的错误请求被采样
  
  // 验证关键服务采样率
  let critical_sampled_count = critical_service_requests.filter(fn(decision) { decision.1 }).length()
  let critical_sampling_rate_actual = if critical_service_requests.length() > 0 {
    critical_sampled_count.to_double() / critical_service_requests.length().to_double()
  } else {
    0.0
  }
  assert_eq(critical_sampling_rate_actual >= 0.3, true)  // 至少30%的关键服务请求被采样
  
  // 验证正常请求采样率
  let normal_sampled_count = normal_requests.filter(fn(decision) { decision.1 }).length()
  let normal_sampling_rate_actual = if normal_requests.length() > 0 {
    normal_sampled_count.to_double() / normal_requests.length().to_double()
  } else {
    0.0
  }
  assert_eq(normal_sampling_rate_actual <= 0.15, true)  // 正常请求采样率不超过15%
}

test "sampling_strategy_composite" {
  // 测试复合采样策略（多种策略的组合）
  
  let total_operations = 3000
  let base_probability = 0.1  // 基础概率采样10%
  let high_frequency_threshold = 100  // 高频操作阈值
  let low_frequency_threshold = 10    // 低频操作阈值
  let latency_threshold_ms = 1000     // 延迟阈值
  
  // 模拟不同类型的操作
  let operations = []
  let mut i = 0
  
  while i < total_operations {
    let operation_name = "op_" + (i % 50).to_string()  // 50种不同的操作
    let operation_frequency = [5, 15, 50, 120, 200][i % 5]  // 不同频率
    let latency_ms = [50, 200, 800, 1500, 3000][i % 5]     // 不同延迟
    let error_rate = [0.0, 0.01, 0.05, 0.1, 0.2][i % 5]    // 不同错误率
    
    let operation = {
      "name": operation_name,
      "frequency": operation_frequency,
      "latency_ms": latency_ms,
      "error_rate": error_rate,
      "trace_id": "trace_" + i.to_string()
    }
    operations.push(operation)
    i = i + 1
  }
  
  // 复合采样策略：结合概率、频率、延迟和错误率
  let composite_sampling_decisions = []
  let mut j = 0
  
  while j < operations.length() {
    let operation = operations[j]
    let mut final_sampling_rate = base_probability
    
    // 频率调整：高频操作降低采样率，低频操作提高采样率
    if operation["frequency"] >= high_frequency_threshold {
      final_sampling_rate = final_sampling_rate * 0.5  // 高频操作采样率减半
    } else if operation["frequency"] <= low_frequency_threshold {
      final_sampling_rate = final_sampling_rate * 2.0  // 低频操作采样率翻倍
    }
    
    // 延迟调整：高延迟操作提高采样率
    if operation["latency_ms"] >= latency_threshold_ms {
      final_sampling_rate = final_sampling_rate * 3.0  // 高延迟操作采样率提高3倍
    }
    
    // 错误率调整：高错误率操作提高采样率
    if operation["error_rate"] >= 0.05 {
      final_sampling_rate = final_sampling_rate * (1.0 + operation["error_rate"] * 10.0)
    }
    
    // 确保采样率在合理范围内
    if final_sampling_rate > 1.0 {
      final_sampling_rate = 1.0
    } else if final_sampling_rate < 0.01 {
      final_sampling_rate = 0.01
    }
    
    // 最终采样决策
    let hash_value = (j.to_int64() * 2654435761L) % 100L
    let should_sample = hash_value < (final_sampling_rate * 100.0).to_int64()
    
    composite_sampling_decisions.push((operation, should_sample, final_sampling_rate))
    j = j + 1
  }
  
  // 验证采样决策数量
  assert_eq(composite_sampling_decisions.length(), total_operations)
  
  // 分析不同类别的采样效果
  let high_latency_ops = composite_sampling_decisions.filter(fn(decision) { 
    decision.0["latency_ms"] >= latency_threshold_ms 
  })
  let high_freq_ops = composite_sampling_decisions.filter(fn(decision) { 
    decision.0["frequency"] >= high_frequency_threshold 
  })
  let low_freq_ops = composite_sampling_decisions.filter(fn(decision) { 
    decision.0["frequency"] <= low_frequency_threshold 
  })
  let high_error_ops = composite_sampling_decisions.filter(fn(decision) { 
    decision.0["error_rate"] >= 0.05 
  })
  
  // 验证高延迟操作采样率更高
  let high_latency_sampled_rate = high_latency_ops.filter(fn(decision) { decision.1 }).length().to_double() / 
                                 high_latency_ops.length().to_double()
  let overall_sampled_rate = composite_sampling_decisions.filter(fn(decision) { decision.1 }).length().to_double() / 
                             composite_sampling_decisions.length().to_double()
  assert_eq(high_latency_sampled_rate > overall_sampled_rate, true)
  
  // 验证高频操作采样率更低
  let high_freq_sampled_rate = high_freq_ops.filter(fn(decision) { decision.1 }).length().to_double() / 
                              high_freq_ops.length().to_double()
  let low_freq_sampled_rate = low_freq_ops.filter(fn(decision) { decision.1 }).length().to_double() / 
                             low_freq_ops.length().to_double()
  assert_eq(high_freq_sampled_rate < low_freq_sampled_rate, true)
  
  // 验证高错误率操作采样率更高
  let high_error_sampled_rate = high_error_ops.filter(fn(decision) { decision.1 }).length().to_double() / 
                               high_error_ops.length().to_double()
  assert_eq(high_error_sampled_rate > overall_sampled_rate, true)
}

test "sampling_strategy_parent_based" {
  // 测试基于父Span的采样策略
  
  let total_spans = 2000
  let root_span_sampling_rate = 0.2  // 根Span采样率20%
  let child_span_inheritance_rate = 0.8  // 子Span继承率80%
  
  // 模拟Span树结构
  let span_trees = []
  let tree_count = 200
  let spans_per_tree = total_spans / tree_count
  
  let mut i = 0
  while i < tree_count {
    let mut current_tree = []
    let root_trace_id = "trace_" + i.to_string()
    
    // 创建根Span
    let root_span = {
      "span_id": "span_" + i.to_string() + "_0",
      "parent_span_id": "",
      "trace_id": root_trace_id,
      "is_root": true,
      "tree_index": i
    }
    current_tree.push(root_span)
    
    // 创建子Span
    let mut j = 1
    while j < spans_per_tree {
      let child_span = {
        "span_id": "span_" + i.to_string() + "_" + j.to_string(),
        "parent_span_id": "span_" + i.to_string() + "_" + (j - 1).to_string(),
        "trace_id": root_trace_id,
        "is_root": false,
        "tree_index": i
      }
      current_tree.push(child_span)
      j = j + 1
    }
    
    span_trees.push(current_tree)
    i = i + 1
  }
  
  // 基于父Span的采样决策
  let parent_based_sampling_decisions = []
  let mut k = 0
  
  while k < span_trees.length() {
    let current_tree = span_trees[k]
    let mut root_sampled = false
    
    // 决策根Span的采样
    let root_hash_value = (k.to_int64() * 2654435761L) % 100L
    root_sampled = root_hash_value < (root_span_sampling_rate * 100.0).to_int64()
    
    // 处理树中的所有Span
    let mut l = 0
    while l < current_tree.length() {
      let span = current_tree[l]
      
      let should_sample = if span["is_root"] {
        root_sampled
      } else {
        // 子Span根据父Span的采样状态和继承率决定
        if root_sampled {
          let inheritance_hash = ((k * 1000 + l).to_int64() * 2654435761L) % 100L
          inheritance_hash < (child_span_inheritance_rate * 100.0).to_int64()
        } else {
          false  // 父Span未采样，子Span也不采样
        }
      }
      
      parent_based_sampling_decisions.push((span, should_sample, root_sampled))
      l = l + 1
    }
    
    k = k + 1
  }
  
  // 验证采样决策数量
  assert_eq(parent_based_sampling_decisions.length(), total_spans)
  
  // 分析采样效果
  let root_spans = parent_based_sampling_decisions.filter(fn(decision) { decision.0["is_root"] })
  let child_spans = parent_based_sampling_decisions.filter(fn(decision) { !decision.0["is_root"] })
  let sampled_root_spans = root_spans.filter(fn(decision) { decision.1 })
  let sampled_child_spans = child_spans.filter(fn(decision) { decision.1 })
  
  // 验证根Span采样率
  let root_sampling_rate_actual = sampled_root_spans.length().to_double() / root_spans.length().to_double()
  assert_eq(root_sampling_rate_actual >= 0.15 && root_sampling_rate_actual <= 0.25, true)
  
  // 验证子Span采样逻辑
  let mut correct_child_sampling = 0
  let mut m = 0
  while m < child_spans.length() {
    let (child_span, should_sample, root_sampled) = child_spans[m]
    
    // 如果根Span未采样，子Span也不应该被采样
    if !root_sampled && !should_sample {
      correct_child_sampling = correct_child_sampling + 1
    }
    // 如果根Span被采样，子Span应该根据继承率决定
    else if root_sampled {
      // 这里我们只检查逻辑正确性，不检查具体比例
      correct_child_sampling = correct_child_sampling + 1
    }
    
    m = m + 1
  }
  
  assert_eq(correct_child_sampling, child_spans.length())
  
  // 验证采样的Span树完整性：采样的根Span应该有相应的子Span
  let sampled_trees = {}
  let mut n = 0
  while n < parent_based_sampling_decisions.length() {
    let (span, should_sample, _) = parent_based_sampling_decisions[n]
    let tree_index = span["tree_index"].to_int()
    
    if should_sample {
      if sampled_trees.contains(tree_index) {
        let current_count = sampled_trees[tree_index]?
        sampled_trees[tree_index] = current_count + 1
      } else {
        sampled_trees[tree_index] = 1
      }
    }
    
    n = n + 1
  }
  
  // 验证至少有一些树被部分采样
  assert_eq(sampled_trees.length() > 0, true)
}