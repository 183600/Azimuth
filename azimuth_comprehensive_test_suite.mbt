// Azimuth 综合测试套件
// 使用标准 MoonBit 测试语法，包含 8 个高质量测试用例

test "core_arithmetic_functions" {
  // 测试核心算术函数
  let add_result = azimuth.add(15, 25)
  let multiply_result = azimuth.multiply(6, 7)
  let subtract_result = azimuth.subtract(50, 20)
  let divide_ceil_result = azimuth.divide_with_ceil(17, 5)
  
  // 验证基本算术运算
  azimuth.assert_eq(add_result, 40)
  azimuth.assert_eq(multiply_result, 42)
  azimuth.assert_eq(subtract_result, 30)
  azimuth.assert_eq(divide_ceil_result, 4)
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  let greeting_en = azimuth.greet("World")
  let greeting_cn = azimuth.greet("世界")
  let greeting_empty = azimuth.greet("")
  let greeting_special = azimuth.greet("!@#$%")
  
  // 验证不同输入的问候语
  azimuth.assert_eq_string(greeting_en, "Hello, World!")
  azimuth.assert_eq_string(greeting_cn, "Hello, 世界!")
  azimuth.assert_eq_string(greeting_empty, "Hello, !")
  azimuth.assert_eq_string(greeting_special, "Hello, !@#$%!")
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 正常情况
  let normal1 = azimuth.divide_with_ceil(10, 3)
  let normal2 = azimuth.divide_with_ceil(12, 4)
  
  // 零值情况
  let zero_dividend = azimuth.divide_with_ceil(0, 5)
  let zero_divisor = azimuth.divide_with_ceil(10, 0)
  let both_zero = azimuth.divide_with_ceil(0, 0)
  
  // 负数情况
  let neg_dividend = azimuth.divide_with_ceil(-10, 3)
  let neg_divisor = azimuth.divide_with_ceil(10, -3)
  let both_negative = azimuth.divide_with_ceil(-10, -3)
  
  // 验证结果
  azimuth.assert_eq(normal1, 4)
  azimuth.assert_eq(normal2, 3)
  azimuth.assert_eq(zero_dividend, 0)
  azimuth.assert_eq(zero_divisor, 0)
  azimuth.assert_eq(both_zero, 0)
  azimuth.assert_eq(neg_dividend, -3)
  azimuth.assert_eq(neg_divisor, 3)
  azimuth.assert_eq(both_negative, 4)
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列: 2, 5, 8, 11, 14
  let term1 = 2
  let diff = 3
  let term2 = azimuth.add(term1, diff)
  let term3 = azimuth.add(term2, diff)
  let term4 = azimuth.add(term3, diff)
  let term5 = azimuth.add(term4, diff)
  
  // 数列求和
  let sequence_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(term1, term2), term3), term4), term5)
  
  // 等比数列: 3, 6, 12, 24, 48
  let geo_term1 = 3
  let geo_term2 = azimuth.multiply(geo_term1, 2)
  let geo_term3 = azimuth.multiply(geo_term2, 2)
  let geo_term4 = azimuth.multiply(geo_term3, 2)
  let geo_term5 = azimuth.multiply(geo_term4, 2)
  
  // 验证计算结果
  azimuth.assert_eq(term2, 5)
  azimuth.assert_eq(term3, 8)
  azimuth.assert_eq(term4, 11)
  azimuth.assert_eq(term5, 14)
  azimuth.assert_eq(sequence_sum, 40)
  azimuth.assert_eq(geo_term2, 6)
  azimuth.assert_eq(geo_term3, 12)
  azimuth.assert_eq(geo_term4, 24)
  azimuth.assert_eq(geo_term5, 48)
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  // 商品定价计算
  let cost_price = 50
  let markup_percentage = 30
  let tax_rate = 8
  
  // 计算加价
  let markup_amount = azimuth.divide_with_ceil(azimuth.multiply(cost_price, markup_percentage), 100)
  let selling_price_before_tax = azimuth.add(cost_price, markup_amount)
  
  // 计算税费
  let tax_amount = azimuth.divide_with_ceil(azimuth.multiply(selling_price_before_tax, tax_rate), 100)
  let final_selling_price = azimuth.add(selling_price_before_tax, tax_amount)
  
  // 批量折扣计算
  let bulk_quantity = 10
  let discount_threshold = 5
  let discount_percentage = 15
  let discount_applicable = bulk_quantity > discount_threshold
  let discount_amount = azimuth.divide_with_ceil(azimuth.multiply(final_selling_price, discount_percentage), 100)
  let discounted_price = azimuth.subtract(final_selling_price, discount_amount)
  
  // 验证计算结果
  azimuth.assert_eq(markup_amount, 15)
  azimuth.assert_eq(selling_price_before_tax, 65)
  azimuth.assert_eq(tax_amount, 6)
  azimuth.assert_eq(final_selling_price, 71)
  azimuth.assert_eq(discount_amount, 11)
  azimuth.assert_eq(discounted_price, 60)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  // O(1) 常数时间操作
  let constant_op = azimuth.add(100, 200)
  
  // O(n) 线性时间操作模拟 - 求和
  let n = 8
  let linear_sum1 = azimuth.add(1, 2)
  let linear_sum2 = azimuth.add(linear_sum1, 3)
  let linear_sum3 = azimuth.add(linear_sum2, 4)
  let linear_sum4 = azimuth.add(linear_sum3, 5)
  let linear_sum5 = azimuth.add(linear_sum4, 6)
  let linear_sum6 = azimuth.add(linear_sum5, 7)
  let linear_sum7 = azimuth.add(linear_sum6, 8)
  
  // O(n²) 二次时间操作模拟 - 嵌套循环
  let matrix_size = 4
  let total_operations = azimuth.multiply(matrix_size, matrix_size)
  
  // O(log n) 对数时间操作模拟 - 二分查找步骤
  let array_size = 32
  let binary_step1 = azimuth.divide_with_ceil(array_size, 2)
  let binary_step2 = azimuth.divide_with_ceil(binary_step1, 2)
  let binary_step3 = azimuth.divide_with_ceil(binary_step2, 2)
  
  // 验证结果
  azimuth.assert_eq(constant_op, 300)
  azimuth.assert_eq(linear_sum7, 36)  // 1+2+...+8 = 36
  azimuth.assert_eq(total_operations, 16)
  azimuth.assert_eq(binary_step1, 16)
  azimuth.assert_eq(binary_step2, 8)
  azimuth.assert_eq(binary_step3, 4)
}

test "physics_simulation_calculations" {
  // 测试物理模拟计算
  // 运动学计算
  let initial_velocity = 10
  let acceleration = 3
  let time = 4
  
  // 计算最终速度: v = u + at
  let final_velocity = azimuth.add(initial_velocity, azimuth.multiply(acceleration, time))
  
  // 计算位移: s = ut + 0.5at²
  let displacement_part1 = azimuth.multiply(initial_velocity, time)
  let displacement_part2 = azimuth.divide_with_ceil(azimuth.multiply(acceleration, azimuth.multiply(time, time)), 2)
  let total_displacement = azimuth.add(displacement_part1, displacement_part2)
  
  // 动能计算: KE = 0.5mv²
  let mass = 15
  let velocity_squared = azimuth.multiply(final_velocity, final_velocity)
  let kinetic_energy = azimuth.divide_with_ceil(azimuth.multiply(mass, velocity_squared), 2)
  
  // 势能计算: PE = mgh
  let height = 20
  let gravity = 10
  let potential_energy = azimuth.multiply(mass, azimuth.multiply(gravity, height))
  
  // 验证计算结果
  azimuth.assert_eq(final_velocity, 22)
  azimuth.assert_eq(displacement_part1, 40)
  azimuth.assert_eq(displacement_part2, 24)
  azimuth.assert_eq(total_displacement, 64)
  azimuth.assert_eq(kinetic_energy, 3630)
  azimuth.assert_eq(potential_energy, 3000)
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  // 栈操作模拟
  let stack_size = 0
  let stack_push1 = 10
  let stack_push2 = 20
  let stack_push3 = 30
  
  // 模拟push操作
  let size_after_push1 = azimuth.add(stack_size, 1)
  let size_after_push2 = azimuth.add(size_after_push1, 1)
  let size_after_push3 = azimuth.add(size_after_push2, 1)
  
  // 模拟pop操作
  let size_after_pop1 = azimuth.subtract(size_after_push3, 1)
  let size_after_pop2 = azimuth.subtract(size_after_pop1, 1)
  let size_after_pop3 = azimuth.subtract(size_after_pop2, 1)
  
  // 队列操作模拟
  let queue_size = 0
  let queue_enqueue1 = 100
  let queue_enqueue2 = 200
  
  // 模拟enqueue操作
  let queue_size_after_enqueue1 = azimuth.add(queue_size, 1)
  let queue_size_after_enqueue2 = azimuth.add(queue_size_after_enqueue1, 1)
  
  // 模拟dequeue操作
  let queue_size_after_dequeue1 = azimuth.subtract(queue_size_after_enqueue2, 1)
  
  // 验证结果
  azimuth.assert_eq(size_after_push1, 1)
  azimuth.assert_eq(size_after_push2, 2)
  azimuth.assert_eq(size_after_push3, 3)
  azimuth.assert_eq(size_after_pop1, 2)
  azimuth.assert_eq(size_after_pop2, 1)
  azimuth.assert_eq(size_after_pop3, 0)
  azimuth.assert_eq(queue_size_after_enqueue1, 1)
  azimuth.assert_eq(queue_size_after_enqueue2, 2)
  azimuth.assert_eq(queue_size_after_dequeue1, 1)
}