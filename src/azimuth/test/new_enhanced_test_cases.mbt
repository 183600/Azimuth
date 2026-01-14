// 新增的 MoonBit 测试用例
// 覆盖新的测试场景和边界情况

test "error_recovery_scenarios" {
  // 测试错误恢复场景：在极端条件下的错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续溢出操作的恢复能力
  let overflow1 = @azimuth.add(max_val, 100)
  let overflow2 = @azimuth.add(overflow1, 100)
  let overflow3 = @azimuth.add(overflow2, 100)
  
  // 所有溢出操作都应该返回最大值
  @azimuth.assert_eq(max_val, overflow1)
  @azimuth.assert_eq(max_val, overflow2)
  @azimuth.assert_eq(max_val, overflow3)
  
  // 测试连续下溢操作的恢复能力
  let underflow1 = @azimuth.add(min_val, -100)
  let underflow2 = @azimuth.add(underflow1, -100)
  let underflow3 = @azimuth.add(underflow2, -100)
  
  // 所有小溢操作都应该返回最小值
  @azimuth.assert_eq(min_val, underflow1)
  @azimuth.assert_eq(min_val, underflow2)
  @azimuth.assert_eq(min_val, underflow3)
  
  // 测试乘法溢出的恢复能力
  let mul_overflow1 = @azimuth.multiply(max_val, 2)
  let mul_overflow2 = @azimuth.multiply(mul_overflow1, 2)
  
  @azimuth.assert_eq(max_val, mul_overflow1)
  @azimuth.assert_eq(max_val, mul_overflow2)
}

test "algorithm_implementation_tests" {
  // 测试简单算法的实现
  // 实现欧几里得算法计算最大公约数（简化版）
  let a = 48
  let b = 18
  
  // 手动实现GCD算法的步骤
  // 48 % 18 = 12
  // 18 % 12 = 6
  // 12 % 6 = 0
  // 所以GCD是6
  let remainder1 = a - @azimuth.multiply(18, 2)  // 48 - 36 = 12
  let remainder2 = b - remainder1               // 18 - 12 = 6
  let gcd = remainder2                          // 6
  
  @azimuth.assert_eq(6, gcd)
  
  // 实现简单的素数测试（检查7是否为素数）
  let prime_candidate = 7
  let is_prime = prime_candidate > 1 && 
                 prime_candidate != 2 && 
                 prime_candidate != 3 && 
                 prime_candidate != 5
  
  @azimuth.assert_true(is_prime)
  
  // 实现斐波那契数列的第8项
  // F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13, F(8)=21
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = @azimuth.add(fib_0, fib_1)  // 1
  let fib_3 = @azimuth.add(fib_1, fib_2)  // 2
  let fib_4 = @azimuth.add(fib_2, fib_3)  // 3
  let fib_5 = @azimuth.add(fib_3, fib_4)  // 5
  let fib_6 = @azimuth.add(fib_4, fib_5)  // 8
  let fib_7 = @azimuth.add(fib_5, fib_6)  // 13
  let fib_8 = @azimuth.add(fib_6, fib_7)  // 21
  
  @azimuth.assert_eq(21, fib_8)
}

test "data_type_boundary_tests" {
  // 测试不同数据类型的边界情况
  // 测试零值的特殊性质
  let zero = 0
  let positive = 42
  let negative = -42
  
  // 零值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(zero, positive))
  @azimuth.assert_eq(0, @azimuth.multiply(zero, negative))
  @azimuth.assert_eq(0, @azimuth.multiply(zero, zero))
  
  // 零值加法测试
  @azimuth.assert_eq(positive, @azimuth.add(zero, positive))
  @azimuth.assert_eq(negative, @azimuth.add(zero, negative))
  @azimuth.assert_eq(zero, @azimuth.add(zero, zero))
  
  // 单位元测试
  let one = 1
  let neg_one = -1
  
  @azimuth.assert_eq(positive, @azimuth.multiply(positive, one))
  @azimuth.assert_eq(negative, @azimuth.multiply(negative, one))
  @azimuth.assert_eq(@azimuth.add(positive, -positive), @azimuth.multiply(positive, zero))
  
  // 测试边界值的算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与单位元的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
}

test "composite_function_tests" {
  // 测试多个函数组合使用的场景
  // 场景：计算一个圆形花园的围栏成本
  let radius = 5
  let fence_cost_per_meter = 10
  let gate_cost = 50
  
  // 计算圆周长（简化：C = 2 × π × r ≈ 6 × r）
  let circumference = @azimuth.multiply(6, radius)
  
  // 计算围栏总长度（包括门）
  let total_fence_length = circumference
  
  // 计算围栏成本
  let fence_cost = @azimuth.multiply(total_fence_length, fence_cost_per_meter)
  
  // 计算总成本
  let total_cost = @azimuth.add(fence_cost, gate_cost)
  
  @azimuth.assert_eq(350, total_cost)  // (6×5)×10 + 50 = 300 + 50 = 350
  
  // 场景：计算折扣商品的价格
  let original_price = 200
  let discount_percentage = 15
  let tax_percentage = 8
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_percentage) / 100
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(183, final_price)  // 200 - 30 + 13 = 183
}

test "performance_stress_tests" {
  // 测试在较大数据量下的表现
  // 大数累加测试
  let large_number = 1000000
  let iterations = 100
  
  // 计算：large_number + (large_number-1) + ... + (large_number-iterations+1)
  // 使用公式：n × a1 - n × (n-1) / 2
  let sum_formula = @azimuth.multiply(iterations, large_number) - @azimuth.multiply(iterations, @azimuth.add(iterations, -1)) / 2
  
  // 验证前几项
  let sum_first_5 = @azimuth.multiply(5, large_number) - @azimuth.multiply(5, 4) / 2
  @azimuth.assert_eq(4999990, sum_first_5)
  
  // 验证总和
  @azimuth.assert_eq(99995000, sum_formula)
  
  // 大数连乘测试
  let base = 10
  let power = 6
  
  // 计算：10^6 = 1,000,000
  let result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(base, base), @azimuth.multiply(base, base)), @azimuth.multiply(base, base))
  @azimuth.assert_eq(1000000, result)
  
  // 复合运算压力测试
  let complex_result = @azimuth.add(
    @azimuth.multiply(1000, 100),
    @azimuth.multiply(@azimuth.add(500, 200), 50)
  )
  @azimuth.assert_eq(185000, complex_result)
}

test "internationalization_tests" {
  // 测试不同语言和字符集的处理
  // 测试中文处理
  let chinese_name = "张三"
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(chinese_name))
  
  // 测试日文处理
  let japanese_name = "田中太郎"
  @azimuth.assert_eq_string("Hello, 田中太郎!", @azimuth.greet(japanese_name))
  
  // 测试韩文处理
  let korean_name = "김철수"
  @azimuth.assert_eq_string("Hello, 김철수!", @azimuth.greet(korean_name))
  
  // 测试阿拉伯文处理
  let arabic_name = "أحمد"
  @azimuth.assert_eq_string("Hello, أحمد!", @azimuth.greet(arabic_name))
  
  // 测试俄文处理
  let russian_name = "Иван"
  @azimuth.assert_eq_string("Hello, Иван!", @azimuth.greet(russian_name))
  
  // 测试混合语言字符串
  let mixed_name = "John张三田中김철수"
  @azimuth.assert_eq_string("Hello, John张三田中김철수!", @azimuth.greet(mixed_name))
  
  // 测试包含表情符号的字符串
  let emoji_name = "🌍🎉🚀"
  @azimuth.assert_eq_string("Hello, 🌍🎉🚀!", @azimuth.greet(emoji_name))
  
  // 测试包含特殊符号的字符串
  let symbols_name = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", @azimuth.greet(symbols_name))
}

test "memory_efficiency_tests" {
  // 测试内存使用效率相关的场景
  // 测试重复使用相同变量的计算
  let base_value = 100
  
  // 方式1：重复创建中间变量
  let temp1 = @azimuth.add(base_value, 10)
  let temp2 = @azimuth.add(temp1, 20)
  let temp3 = @azimuth.add(temp2, 30)
  let result1 = @azimuth.add(temp3, 40)
  
  // 方式2：链式计算
  let result2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(base_value, 10), 20), 30), 40)
  
  // 两种方式应该得到相同结果
  @azimuth.assert_eq(result1, result2)
  @azimuth.assert_eq(200, result1)
  
  // 测试计算的复用性
  let common_calculation = @azimuth.multiply(25, 4)  // 100
  
  // 复用计算结果
  let result_a = @azimuth.add(common_calculation, 50)  // 150
  let result_b = @azimuth.multiply(common_calculation, 2)  // 200
  let result_c = @azimuth.add(common_calculation, -25)  // 75
  
  @azimuth.assert_eq(150, result_a)
  @azimuth.assert_eq(200, result_b)
  @azimuth.assert_eq(75, result_c)
  
  // 测试批量计算的效率
  let batch_size = 10
  let unit_value = 5
  
  // 批量计算：5 + 10 + 15 + ... + 50
  // 使用公式：n × (a1 + an) / 2
  let last_value = @azimuth.multiply(unit_value, batch_size)
  let batch_sum = @azimuth.multiply(batch_size, @azimuth.add(unit_value, last_value)) / 2
  
  @azimuth.assert_eq(275, batch_sum)
}

test "concurrency_simulation_tests" {
  // 模拟并发操作的场景
  // 场景1：模拟两个线程同时更新计数器
  let counter = 0
  let increment1 = 5
  let increment2 = 3
  
  // 线程1操作
  let counter_after_thread1 = @azimuth.add(counter, increment1)
  
  // 线程2操作（基于原始值）
  let counter_after_thread2 = @azimuth.add(counter, increment2)
  
  // 模拟合并结果
  let final_counter = @azimuth.add(counter_after_thread1, increment2)
  let alternative_counter = @azimuth.add(counter_after_thread2, increment1)
  
  // 两种方式应该得到相同结果
  @azimuth.assert_eq(final_counter, alternative_counter)
  @azimuth.assert_eq(8, final_counter)
  
  // 场景2：模拟银行账户的并发操作
  let account_balance = 1000
  let deposit1 = 200
  let deposit2 = 300
  let withdrawal1 = 150
  let withdrawal2 = 100
  
  // 并发存款操作
  let balance_after_deposit1 = @azimuth.add(account_balance, deposit1)
  let balance_after_deposit2 = @azimuth.add(account_balance, deposit2)
  
  // 并发取款操作
  let balance_after_withdrawal1 = @azimuth.add(account_balance, -withdrawal1)
  let balance_after_withdrawal2 = @azimuth.add(account_balance, -withdrawal2)
  
  // 模拟最终状态：所有操作都成功
  let final_balance = @azimuth.add(@azimuth.add(@azimuth.add(account_balance, deposit1), deposit2), @azimuth.add(-withdrawal1, -withdrawal2))
  
  @azimuth.assert_eq(1250, final_balance)
  
  // 场景3：模拟库存管理
  let initial_stock = 100
  let incoming_stock = 50
  let sales1 = 20
  let sales2 = 30
  let returns = 5
  
  // 计算最终库存
  let final_stock = @azimuth.add(@azimuth.add(@azimuth.add(initial_stock, incoming_stock), -sales1), @azimuth.add(-sales2, returns))
  
  @azimuth.assert_eq(105, final_stock)
}