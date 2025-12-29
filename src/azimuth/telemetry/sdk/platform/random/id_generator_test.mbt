// 平台随机数生成器测试 - 用于测试跨平台随机数和ID生成功能

test "random_number_generator_basic" {
  // 测试基本随机数生成功能
  
  enum RNGType {
    SystemRandom
    CryptographicSecure
    PseudoRandom
    HardwareRandom
  }
  
  struct RNGConfig {
    rng_type : RNGType
    seed : Option[Int64]
    quality : String
    performance : String
  }
  
  let rng_configs = [
    {
      rng_type: SystemRandom,
      seed: None,
      quality: "medium",
      performance: "high"
    },
    {
      rng_type: CryptographicSecure,
      seed: None,
      quality: "high",
      performance: "medium"
    },
    {
      rng_type: PseudoRandom,
      seed: Some(12345L),
      quality: "low",
      performance: "very_high"
    },
    {
      rng_type: HardwareRandom,
      seed: None,
      quality: "very_high",
      performance: "medium"
    }
  ]
  
  // 验证RNG配置
  for config in rng_configs {
    match config.rng_type {
      CryptographicSecure | HardwareRandom => {
        assert_eq(config.quality == "high" || config.quality == "very_high", true)
        assert_eq(config.seed, None)
      }
      PseudoRandom => {
        assert_eq(config.seed != None, true)
        assert_eq(config.performance, "very_high")
      }
      SystemRandom => {
        assert_eq(config.quality, "medium")
        assert_eq(config.performance, "high")
      }
    }
  }
  
  // 模拟随机数生成
  let mut pseudo_random_state = 12345L
  
  // 简单的线性同余生成器
  let generate_pseudo_random = fn(state : Int64) : Int64 {
    (state * 1103515245L + 12345L) % 2147483648L
  }
  
  // 生成一些随机数
  let mut random_numbers : Array[Int64] = []
  for i = 0; i < 10; i = i + 1 {
    pseudo_random_state = generate_pseudo_random(pseudo_random_state)
    random_numbers.push(pseudo_random_state)
  }
  
  assert_eq(random_numbers.length(), 10)
  
  // 验证随机数的范围
  for num in random_numbers {
    assert_eq(num >= 0 && num < 2147483648L, true)
  }
  
  // 验证随机数的不同性（简单检查）
  let mut has_duplicates = false
  for i = 0; i < random_numbers.length(); i = i + 1 {
    for j = i + 1; j < random_numbers.length(); j = j + 1 {
      if random_numbers[i] == random_numbers[j] {
        has_duplicates = true
        break
      }
    }
  }
  
  // 在小样本中允许重复，但不应太多
  assert_eq(has_duplicates, false)  // 这个简单的生成器在小样本中通常不会重复
}

test "unique_id_generation" {
  // 测试唯一ID生成
  
  enum IDType {
    UUID
    NanoID
    Snowflake
    ULID
    Custom
  }
  
  struct IDGenerator {
    id_type : IDType
    length : Int
    alphabet : String
    timestamp_included : Bool
  }
  
  let generators = [
    {
      id_type: UUID,
      length: 36,
      alphabet: "0123456789abcdef-",
      timestamp_included: true
    },
    {
      id_type: NanoID,
      length: 21,
      alphabet: "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
      timestamp_included: false
    },
    {
      id_type: Snowflake,
      length: 19,  // 数字字符串长度
      alphabet: "0123456789",
      timestamp_included: true
    },
    {
      id_type: ULID,
      length: 26,
      alphabet: "0123456789ABCDEFGHJKMNPQRSTVWXYZ",
      timestamp_included: true
    }
  ]
  
  // 验证生成器配置
  for generator in generators {
    assert_eq(generator.length > 0, true)
    assert_eq(generator.alphabet.length() > 0, true)
    
    match generator.id_type {
      UUID => assert_eq(generator.length, 36)
      NanoID => assert_eq(generator.length, 21)
      Snowflake => assert_eq(generator.alphabet, "0123456789")
      ULID => assert_eq(generator.length, 26)
      _ => ()
    }
  }
  
  // 模拟UUID生成
  let generate_uuid = fn() : String {
    "12345678-1234-5678-9012-123456789012"
  }
  
  let uuid = generate_uuid()
  assert_eq(uuid.length(), 36)
  assert_eq(uuid.contains("-"), true)
  
  // 验证UUID格式
  let uuid_parts = uuid.split("-")
  assert_eq(uuid_parts.length(), 5)
  assert_eq(uuid_parts[0].length(), 8)
  assert_eq(uuid_parts[1].length(), 4)
  assert_eq(uuid_parts[2].length(), 4)
  assert_eq(uuid_parts[3].length(), 4)
  assert_eq(uuid_parts[4].length(), 12)
  
  // 模拟Snowflake ID生成
  let mut snowflake_sequence = 0L
  let machine_id = 1023L  // 10位机器ID
  let generate_snowflake = fn(timestamp : Int64, seq : Int64, machine : Int64) : String {
    let snowflake = (timestamp << 22) | (machine << 12) | seq
    snowflake.to_string()
  }
  
  let base_timestamp = 1609459200000L  // 2021-01-01
  let snowflake1 = generate_snowflake(base_timestamp, snowflake_sequence, machine_id)
  snowflake_sequence = snowflake_sequence + 1
  let snowflake2 = generate_snowflake(base_timestamp, snowflake_sequence, machine_id)
  
  assert_eq(snowflake1.length(), 19)
  assert_eq(snowflake2.length(), 19)
  assert_eq(snowflake1 != snowflake2, true)
  
  // 验证Snowflake ID的时间戳部分
  let snowflake1_num = snowflake1.to_int64()
  let extracted_timestamp = snowflake1_num >> 22
  assert_eq(extracted_timestamp, base_timestamp)
}

test "random_quality_assessment" {
  // 测试随机数质量评估
  
  struct RandomQualityMetrics {
    entropy_bits : Int
    chi_square_statistic : Double
    runs_test : Bool
    frequency_test : Bool
    serial_correlation : Double
  }
  
  let quality_metrics = [
    {
      entropy_bits: 128,
      chi_square_statistic: 15.2,
      runs_test: true,
      frequency_test: true,
      serial_correlation: 0.02
    },
    {
      entropy_bits: 256,
      chi_square_statistic: 8.7,
      runs_test: true,
      frequency_test: true,
      serial_correlation: 0.01
    },
    {
      entropy_bits: 64,
      chi_square_statistic: 23.4,
      runs_test: false,
      frequency_test: true,
      serial_correlation: 0.15
    }
  ]
  
  // 验证质量指标
  for metrics in quality_metrics {
    assert_eq(metrics.entropy_bits > 0, true)
    assert_eq(metrics.chi_square_statistic > 0.0, true)
    assert_eq(metrics.serial_correlation >= -1.0 && metrics.serial_correlation <= 1.0, true)
  }
  
  // 评估随机数质量
  let assess_quality = fn(metrics : RandomQualityMetrics) : String {
    if metrics.entropy_bits >= 256 && 
       metrics.chi_square_statistic < 15.0 && 
       metrics.runs_test && 
       metrics.frequency_test && 
       @abs(metrics.serial_correlation) < 0.05 {
      "excellent"
    } else if metrics.entropy_bits >= 128 && 
              metrics.chi_square_statistic < 25.0 && 
              metrics.frequency_test && 
              @abs(metrics.serial_correlation) < 0.1 {
      "good"
    } else if metrics.entropy_bits >= 64 && metrics.frequency_test {
      "acceptable"
    } else {
      "poor"
    }
  }
  
  let qualities = quality_metrics.map(assess_quality)
  assert_eq(qualities[0], "good")
  assert_eq(qualities[1], "excellent")
  assert_eq(qualities[2], "acceptable")
  
  // 测试随机数分布均匀性
  let generate_test_sequence = fn(size : Int) : Array[Int] {
    let mut sequence : Array[Int] = []
    let mut state = 12345L
    
    for i = 0; i < size; i = i + 1 {
      state = (state * 1103515245L + 12345L) % 2147483648L
      sequence.push((state % 10).to_int())  // 0-9的数字
    }
    
    sequence
  }
  
  let test_sequence = generate_test_sequence(1000)
  
  // 统计每个数字的出现次数
  let mut counts : Array[Int> = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  for num in test_sequence {
    if num >= 0 && num < 10 {
      counts[num] = counts[num] + 1
    }
  }
  
  // 验证分布的均匀性（每个数字应该大约出现100次）
  let expected_count = test_sequence.length() / 10
  let tolerance = expected_count / 5  // 20%的容差
  
  for count in counts {
    let deviation = @abs(count - expected_count)
    assert_eq(deviation <= tolerance, true)
  }
}

test "id_collision_detection" {
  // 测试ID碰撞检测
  
  struct CollisionTestResult {
    total_generated : Int
    collisions_found : Int
    collision_rate : Double
    test_duration_ms : Int64
  }
  
  // 模拟碰撞测试
  let collision_test = fn(generator : String -> String, count : Int) : CollisionTestResult {
    let mut ids : Array[String> = []
    let mut collisions = 0
    
    for i = 0; i < count; i = i + 1 {
      let id = generator()
      
      // 检查是否已存在
      let mut found = false
      for existing_id in ids {
        if existing_id == id {
          found = true
          break
        }
      }
      
      if found {
        collisions = collisions + 1
      } else {
        ids.push(id)
      }
    }
    
    {
      total_generated: count,
      collisions_found: collisions,
      collision_rate: collisions.to_double() / count.to_double(),
      test_duration_ms: 100L  // 模拟时间
    }
  }
  
  // 测试不同ID生成器的碰撞率
  let uuid_generator = fn() : String {
    "12345678-1234-5678-9012-" + (@rand().to_string())
  }
  
  let short_id_generator = fn() : String {
    (@rand().to_string())[-4:]  // 4位数字ID
  }
  
  let uuid_test = collision_test(uuid_generator, 1000)
  let short_id_test = collision_test(short_id_generator, 100)
  
  // 验证碰撞测试结果
  assert_eq(uuid_test.total_generated, 1000)
  assert_eq(short_id_test.total_generated, 100)
  assert_eq(uuid_test.collision_rate >= 0.0, true)
  assert_eq(short_id_test.collision_rate >= 0.0, true)
  
  // UUID应该有很低的碰撞率
  assert_eq(uuid_test.collision_rate < 0.001, true)
  
  // 短ID可能有更高的碰撞率
  assert_eq(short_id_test.collision_rate >= 0.0, true)
  
  // 测试生日悖论计算
  let birthday_paradox_probability = fn(possible_values : Int, sample_size : Int) : Double {
    let mut probability = 1.0
    for i = 0; i < sample_size; i = i + 1 {
      probability = probability * ((possible_values - i).to_double() / possible_values.to_double())
    }
    1.0 - probability
  }
  
  // 4位数字ID的生日悖论概率
  let four_digit_prob = birthday_paradox_probability(10000, 100)
  assert_eq(four_digit_prob > 0.3, true)  // 大约39%的碰撞概率
  
  // UUID的生日悖论概率（假设2^128种可能）
  let uuid_prob = birthday_paradox_probability(340282366920938463463374607431768211456, 1000)
  assert_eq(uuid_prob < 0.000001, true)  // 极低的碰撞概率
}

test "random_performance_benchmarks" {
  // 测试随机数生成性能
  
  struct PerformanceBenchmark {
    operation : String
    iterations : Int
    total_time_ns : Int64
    avg_time_ns : Int64
    ops_per_second : Int64
  }
  
  // 模拟性能基准测试
  let benchmarks = [
    {
      operation: "system_random_u64",
      iterations: 1000000,
      total_time_ns: 500000000L,  // 0.5秒
      avg_time_ns: 500L,
      ops_per_second: 2000000L
    },
    {
      operation: "crypto_random_bytes",
      iterations: 100000,
      total_time_ns: 2000000000L, // 2秒
      avg_time_ns: 20000L,
      ops_per_second: 50000L
    },
    {
      operation: "pseudo_random_u32",
      iterations: 10000000,
      total_time_ns: 1000000000L, // 1秒
      avg_time_ns: 100L,
      ops_per_second: 10000000L
    },
    {
      operation: "uuid_generation",
      iterations: 100000,
      total_time_ns: 1500000000L, // 1.5秒
      avg_time_ns: 15000L,
      ops_per_second: 66666L
    }
  ]
  
  // 验证基准测试结果
  for benchmark in benchmarks {
    assert_eq(benchmark.iterations > 0, true)
    assert_eq(benchmark.total_time_ns > 0, true)
    assert_eq(benchmark.avg_time_ns > 0, true)
    assert_eq(benchmark.ops_per_second > 0, true)
    
    // 验证计算一致性
    let calculated_avg = benchmark.total_time_ns / benchmark.iterations.to_int64()
    let calculated_ops = (benchmark.iterations.to_int64() * 1000000000L) / benchmark.total_time_ns
    
    assert_eq(@abs(calculated_avg - benchmark.avg_time_ns) <= 100, true)
    assert_eq(@abs(calculated_ops - benchmark.ops_per_second) <= 10000, true)
  }
  
  // 性能比较
  let system_random = benchmarks[0]
  let crypto_random = benchmarks[1]
  let pseudo_random = benchmarks[2]
  
  // 伪随机应该是最快的
  let is_pseudo_fastest = pseudo_random.avg_time_ns < system_random.avg_time_ns &&
                          pseudo_random.avg_time_ns < crypto_random.avg_time_ns
  assert_eq(is_pseudo_fastest, true)
  
  // 加密随机应该是最慢的
  let is_crypto_slowest = crypto_random.avg_time_ns > system_random.avg_time_ns &&
                          crypto_random.avg_time_ns > pseudo_random.avg_time_ns
  assert_eq(is_crypto_slowest, true)
  
  // 性能比率计算
  let crypto_vs_pseudo_ratio = crypto_random.avg_time_ns / pseudo_random.avg_time_ns
  let system_vs_pseudo_ratio = system_random.avg_time_ns / pseudo_random.avg_time_ns
  
  assert_eq(crypto_vs_pseudo_ratio >= 100, true)  // 加密随机至少慢100倍
  assert_eq(system_vs_pseudo_ratio >= 2, true)    // 系统随机至少慢2倍
  
  // 测试性能阈值
  let max_pseudo_avg_ns = 1000L      // 1微秒
  let max_system_avg_ns = 10000L     // 10微秒
  let max_crypto_avg_ns = 100000L    // 100微秒
  
  assert_eq(pseudo_random.avg_time_ns <= max_pseudo_avg_ns, true)
  assert_eq(system_random.avg_time_ns <= max_system_avg_ns, true)
  assert_eq(crypto_random.avg_time_ns <= max_crypto_avg_ns, true)
}

test "random_seeding_and_state" {
  // 测试随机数种子和状态管理
  
  struct RNGState {
    algorithm : String
    seed : Int64
    internal_state : Int64
    position : Int
  }
  
  // 创建可重现的随机数生成器
  let create_deterministic_rng = fn(seed : Int64) : RNGState {
    {
      algorithm: "xorshift64",
      seed: seed,
      internal_state: seed,
      position: 0
    }
  }
  
  // Xorshift64算法
  let xorshift64_next = fn(state : RNGState) : (Int64, RNGState) {
    let x = state.internal_state
    let x1 = x ^ (x << 13)
    let x2 = x1 ^ (x1 >> 7)
    let x3 = x2 ^ (x2 << 17)
    let new_state = { state | internal_state: x3, position: state.position + 1 }
    (x3, new_state)
  }
  
  // 测试可重现性
  let seed1 = 12345L
  let rng1 = create_deterministic_rng(seed1)
  let rng2 = create_deterministic_rng(seed1)
  
  // 生成序列
  let generate_sequence = fn(initial_state : RNGState, count : Int) : Array<Int64> {
    let mut state = initial_state
    let mut sequence : Array[Int64> = []
    
    for i = 0; i < count; i = i + 1 {
      let (value, new_state) = xorshift64_next(state)
      sequence.push(value)
      state = new_state
    }
    
    sequence
  }
  
  let seq1 = generate_sequence(rng1, 10)
  let seq2 = generate_sequence(rng2, 10)
  
  // 相同种子应该产生相同序列
  assert_eq(seq1.length(), seq2.length())
  for i = 0; i < seq1.length(); i = i + 1 {
    assert_eq(seq1[i], seq2[i])
  }
  
  // 不同种子应该产生不同序列
  let seed2 = 54321L
  let rng3 = create_deterministic_rng(seed2)
  let seq3 = generate_sequence(rng3, 10)
  
  let sequences_different = seq1[0] != seq3[0] || seq1[1] != seq3[1] || seq1[2] != seq3[2]
  assert_eq(sequences_different, true)
  
  // 测试状态保存和恢复
  let (value1, state1) = xorshift64_next(rng1)
  let (value2, state2) = xorshift64_next(state1)
  let (value3, state3) = xorshift64_next(state2)
  
  // 从state2重新开始应该得到相同的结果
  let (value2_again, state2_again) = xorshift64_next(state1)
  let (value3_again, _) = xorshift64_next(state2_again)
  
  assert_eq(value2, value2_again)
  assert_eq(value3, value3_again)
  
  // 测试状态跳转
  let skip_ahead = fn(state : RNGState, steps : Int) : RNGState {
    let mut current_state = state
    for i = 0; i < steps; i = i + 1 {
      let (_, new_state) = xorshift64_next(current_state)
      current_state = new_state
    }
    current_state
  }
  
  let skipped_state = skip_ahead(rng1, 5)
  let (value_after_skip, _) = xorshift64_next(skipped_state)
  
  // 验证跳过5步后的值与序列中第6个值相同
  assert_eq(value_after_skip, seq1[5])
}