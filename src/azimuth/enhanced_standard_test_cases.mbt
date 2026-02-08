// 增强标准测试用例
// 包含10个高质量MoonBit测试用例，使用标准MoonBit测试语法

test "core_arithmetic_operations" {
  // 核心算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-2, @azimuth.add(5, -7))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  
  @azimuth.assert_eq(5, @azimuth.subtract(12, 7))
  @azimuth.assert_eq(-10, @azimuth.subtract(3, 13))
  @azimuth.assert_eq(0, @azimuth.subtract(8, 8))
}

test "advanced_division_operations" {
  // 高级除法运算测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 负数除法测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
}

test "string_function_comprehensive" {
  // 字符串功能综合测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "mathematical_laws_verification" {
  // 数学定律验证测试
  let a = 17
  let b = 23
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 单位元性质
  @azimuth.assert_eq(@azimuth.add(a, 0), a)
  @azimuth.assert_eq(@azimuth.multiply(a, 1), a)
}

test "boundary_conditions_stress" {
  // 边界条件压力测试
  // 零值边界
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 0), 0)
  @azimuth.assert_eq(@azimuth.subtract(0, 0), 0)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 1), 0)
  
  // 单位值边界
  @azimuth.assert_eq(@azimuth.add(1, 0), 1)
  @azimuth.assert_eq(@azimuth.multiply(1, 1), 1)
  @azimuth.assert_eq(@azimuth.subtract(1, 0), 1)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 1), 1)
  
  // 大数边界
  let large_num = 100000
  @azimuth.assert_eq(@azimuth.add(large_num, large_num), 200000)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(large_num, 3), 33334)
  @azimuth.assert_eq(@azimuth.multiply(large_num, 2), 200000)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let base = 100
  let rate = 15
  let periods = 3
  
  // 复合增长计算
  let period1 = @azimuth.add(base, @azimuth.divide_with_ceil(@azimuth.multiply(base, rate), 100))
  let period2 = @azimuth.add(period1, @azimuth.divide_with_ceil(@azimuth.multiply(period1, rate), 100))
  let period3 = @azimuth.add(period2, @azimuth.divide_with_ceil(@azimuth.multiply(period2, rate), 100))
  
  @azimuth.assert_eq(period1, 115)
  @azimuth.assert_eq(period2, 133)
  @azimuth.assert_eq(period3, 153)
  
  // 几何计算
  let length = 8
  let width = 6
  let height = 4
  let volume = @azimuth.multiply(@azimuth.multiply(length, width), height)
  let surface_area = @azimuth.multiply(2, @azimuth.add(@azimuth.add(@azimuth.multiply(length, width), @azimuth.multiply(length, height)), @azimuth.multiply(width, height)))
  
  @azimuth.assert_eq(volume, 192)
  @azimuth.assert_eq(surface_area, 208)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(@azimuth.divide_with_ceil(50, 0), 0)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 0), 0)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-50, 0), 0)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 0), 0)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-1, 0), 0)
  
  // 极值测试
  let max_val = 1000000
  @azimuth.assert_eq(@azimuth.add(max_val, max_val), 2000000)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.subtract(max_val, 0), max_val)
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟测试
  // O(1) 常数时间操作
  let constant_result = @azimuth.add(42, 58)
  @azimuth.assert_eq(constant_result, 100)
  
  // O(n) 线性时间操作模拟 - 累加求和
  let n = 10
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let sum4 = @azimuth.add(sum3, 5)
  let sum5 = @azimuth.add(sum4, 6)
  let sum6 = @azimuth.add(sum5, 7)
  let sum7 = @azimuth.add(sum6, 8)
  let sum8 = @azimuth.add(sum7, 9)
  let sum9 = @azimuth.add(sum8, 10)
  
  @azimuth.assert_eq(sum9, 55)
  
  // O(log n) 对数时间操作模拟 - 二分查找
  let low = 0
  let high = 100
  let target = 75
  let mid = @azimuth.divide_with_ceil(@azimuth.add(low, high), 2)
  @azimuth.assert_eq(mid, 50)
}

test "real_world_applications" {
  // 真实世界应用测试
  // 购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let total = @azimuth.add(subtotal1, subtotal2)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(total, 10), 100)
  let grand_total = @azimuth.add(total, tax)
  
  @azimuth.assert_eq(subtotal1, 75)
  @azimuth.assert_eq(subtotal2, 30)
  @azimuth.assert_eq(total, 105)
  @azimuth.assert_eq(tax, 11)
  @azimuth.assert_eq(grand_total, 116)
  
  // 库存管理
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 200
  
  let weekly_sales = @azimuth.multiply(daily_sales, days)
  let remaining_stock = @azimuth.subtract(initial_stock, weekly_sales)
  let final_stock = @azimuth.add(remaining_stock, new_shipment)
  
  @azimuth.assert_eq(weekly_sales, 175)
  @azimuth.assert_eq(remaining_stock, 325)
  @azimuth.assert_eq(final_stock, 525)
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 栈操作模拟
  let stack_size = 0
  
  // push操作
  let push1 = 10
  let stack_size1 = @azimuth.add(stack_size, 1)
  
  let push2 = 20
  let stack_size2 = @azimuth.add(stack_size1, 1)
  
  let push3 = 30
  let stack_size3 = @azimuth.add(stack_size2, 1)
  
  // pop操作
  let pop1 = 30
  let stack_size_after_pop1 = @azimuth.subtract(stack_size3, 1)
  
  let pop2 = 20
  let stack_size_after_pop2 = @azimuth.subtract(stack_size_after_pop1, 1)
  
  let pop3 = 10
  let stack_size_after_pop3 = @azimuth.subtract(stack_size_after_pop2, 1)
  
  @azimuth.assert_eq(stack_size1, 1)
  @azimuth.assert_eq(stack_size2, 2)
  @azimuth.assert_eq(stack_size3, 3)
  @azimuth.assert_eq(pop1, 30)
  @azimuth.assert_eq(stack_size_after_pop1, 2)
  @azimuth.assert_eq(pop2, 20)
  @azimuth.assert_eq(stack_size_after_pop2, 1)
  @azimuth.assert_eq(pop3, 10)
  @azimuth.assert_eq(stack_size_after_pop3, 0)
  
  // 队列操作模拟
  let queue_size = 0
  
  // enqueue操作
  let enqueue1 = 100
  let queue_size1 = @azimuth.add(queue_size, 1)
  
  let enqueue2 = 200
  let queue_size2 = @azimuth.add(queue_size1, 1)
  
  // dequeue操作
  let dequeue1 = 100
  let queue_size_after_dequeue1 = @azimuth.subtract(queue_size2, 1)
  
  let dequeue2 = 200
  let queue_size_after_dequeue2 = @azimuth.subtract(queue_size_after_dequeue1, 1)
  
  @azimuth.assert_eq(queue_size1, 1)
  @azimuth.assert_eq(queue_size2, 2)
  @azimuth.assert_eq(dequeue1, 100)
  @azimuth.assert_eq(queue_size_after_dequeue1, 1)
  @azimuth.assert_eq(dequeue2, 200)
  @azimuth.assert_eq(queue_size_after_dequeue2, 0)
}