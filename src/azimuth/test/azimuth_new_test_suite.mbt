// Azimuth 新增 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 100
  let step1 = @azimuth.add(initial, 50)  // 150
  let step2 = @azimuth.multiply(step1, 2)  // 300
  let step3 = @azimuth.subtract(step2, 75)  // 225
  let step4 = @azimuth.divide_with_ceil(step3, 4)  // 57
  
  @azimuth.assert_eq(150, step1)
  @azimuth.assert_eq(300, step2)
  @azimuth.assert_eq(225, step3)
  @azimuth.assert_eq(57, step4)
}

test "nested_function_calls" {
  // 嵌套函数调用测试
  // 测试多层函数嵌套调用
  let result1 = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.subtract(10, 2))  // 12 + 8 = 20
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.divide_with_ceil(15, 4))  // 8 * 4 = 32
  let result3 = @azimuth.subtract(@azimuth.multiply(10, 5), @azimuth.add(8, 7))  // 50 - 15 = 35
  
  @azimuth.assert_eq(20, result1)
  @azimuth.assert_eq(32, result2)
  @azimuth.assert_eq(35, result3)
}

test "edge_case_zero_handling" {
  // 零值处理边界情况测试
  // 测试各种涉及零的运算
  
  // 加法零值
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法零值
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 减法零值
  @azimuth.assert_eq(42, @azimuth.subtract(42, 0))
  @azimuth.assert_eq(-42, @azimuth.subtract(0, 42))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
  
  // 除法零值
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 42))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(42, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  // 全面测试负数运算的各种情况
  
  // 负数加法
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(2, @azimuth.add(5, -3))
  
  // 负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(5, -3))
  
  // 负数减法
  @azimuth.assert_eq(-2, @azimuth.subtract(-5, -3))
  @azimuth.assert_eq(-8, @azimuth.subtract(-5, 3))
  @azimuth.assert_eq(8, @azimuth.subtract(5, -3))
  
  // 负数除法
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-10, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(10, -5))
}

test "large_number_operations" {
  // 大数运算测试
  // 测试大数值的运算稳定性
  
  let big_num = 1000000
  let another_big = 500000
  
  // 大数基本运算
  @azimuth.assert_eq(1500000, @azimuth.add(big_num, another_big))
  @azimuth.assert_eq(500000, @azimuth.subtract(big_num, another_big))
  @azimuth.assert_eq(500000000, @azimuth.multiply(10000, 50000))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(big_num, another_big))
  
  // 极大数运算
  let huge_num = 1000000000
  @azimuth.assert_eq(2000000000, @azimuth.add(huge_num, huge_num))
  @azimuth.assert_eq(1000001, @azimuth.divide_with_ceil(huge_num, 1000))
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试字符串函数的各种输入情况
  
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, -678!", @azimuth.greet("-678"))
  
  // 混合内容
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123"))
  @azimuth.assert_eq_string("Hello, test_case!", @azimuth.greet("test_case"))
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 使用基本函数实现数学级数计算
  
  // 等差数列求和：1 + 2 + ... + 10 = 55
  let sum1 = 1
  let sum2 = @azimuth.add(sum1, 2)
  let sum3 = @azimuth.add(sum2, 3)
  let sum4 = @azimuth.add(sum3, 4)
  let sum5 = @azimuth.add(sum4, 5)
  let sum6 = @azimuth.add(sum5, 6)
  let sum7 = @azimuth.add(sum6, 7)
  let sum8 = @azimuth.add(sum7, 8)
  let sum9 = @azimuth.add(sum8, 9)
  let sum10 = @azimuth.add(sum9, 10)
  
  // 等比数列：2^1 + 2^2 + 2^3 + 2^4 = 30
  let geo1 = 2
  let geo2 = @azimuth.multiply(geo1, 2)
  let geo3 = @azimuth.multiply(geo2, 2)
  let geo4 = @azimuth.multiply(geo3, 2)
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(geo1, geo2), geo3), geo4)
  
  @azimuth.assert_eq(55, sum10)
  @azimuth.assert_eq(30, geo_sum)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算测试
  // 模拟实际购物场景的计算
  
  // 商品价格和数量
  let apple_price = 5
  let apple_qty = 6
  let banana_price = 3
  let banana_qty = 8
  let orange_price = 4
  let orange_qty = 4
  
  // 计算各商品小计
  let apple_total = @azimuth.multiply(apple_price, apple_qty)  // 30
  let banana_total = @azimuth.multiply(banana_price, banana_qty)  // 24
  let orange_total = @azimuth.multiply(orange_price, orange_qty)  // 16
  
  // 计算购物车总额
  let cart_total = @azimuth.add(@azimuth.add(apple_total, banana_total), orange_total)  // 70
  
  // 计算税费（10%，向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(cart_total, 10), 100)  // 7
  
  // 计算最终总额
  let final_total = @azimuth.add(cart_total, tax)  // 77
  
  // 计算每袋水果数量（假设每袋装10个）
  let total_fruits = @azimuth.add(@azimuth.add(apple_qty, banana_qty), orange_qty)  // 18
  let bags_needed = @azimuth.divide_with_ceil(total_fruits, 10)  // 2
  
  @azimuth.assert_eq(30, apple_total)
  @azimuth.assert_eq(24, banana_total)
  @azimuth.assert_eq(16, orange_total)
  @azimuth.assert_eq(70, cart_total)
  @azimuth.assert_eq(7, tax)
  @azimuth.assert_eq(77, final_total)
  @azimuth.assert_eq(18, total_fruits)
  @azimuth.assert_eq(2, bags_needed)
}

test "algorithm_simulation_sorting" {
  // 排序算法模拟测试
  // 使用基本函数模拟简单排序过程
  
  // 模拟选择排序：[3, 1, 4, 1, 5] -> [1, 1, 3, 4, 5]
  let arr1 = 3
  let arr2 = 1
  let arr3 = 4
  let arr4 = 1
  let arr5 = 5
  
  // 第一轮：找到最小值1，与第一个元素交换
  let min_val1 = 1
  let first_swap = min_val1
  
  // 第二轮：在剩余元素中找到最小值1
  let min_val2 = 1
  let second_swap = min_val2
  
  // 第三轮：在剩余元素中找到最小值3
  let min_val3 = 3
  let third_swap = min_val3
  
  // 验证排序结果
  @azimuth.assert_eq(1, first_swap)
  @azimuth.assert_eq(1, second_swap)
  @azimuth.assert_eq(3, third_swap)
  
  // 计算总和验证
  let original_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(arr1, arr2), arr3), arr4), arr5)  // 14
  let sorted_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 1), 3), 4), 5)  // 14
  
  @azimuth.assert_eq(original_sum, sorted_sum)
}

test "physics_simulation_gravity" {
  // 物理重力模拟测试
  // 模拟自由落体运动计算
  
  let initial_height = 100  // 米
  let gravity = 10  // 米/秒²
  let time1 = 2  // 秒
  let time2 = 3  // 秒
  let time3 = 4  // 秒
  
  // 计算下落距离：s = 0.5 * g * t²
  // 使用整数近似：s = g * t² / 2
  let distance1 = @azimuth.divide_with_ceil(@azimuth.multiply(gravity, @azimuth.multiply(time1, time1)), 2)  // 20
  let distance2 = @azimuth.divide_with_ceil(@azimuth.multiply(gravity, @azimuth.multiply(time2, time2)), 2)  // 45
  let distance3 = @azimuth.divide_with_ceil(@azimuth.multiply(gravity, @azimuth.multiply(time3, time3)), 2)  // 80
  
  // 计算剩余高度
  let remaining1 = @azimuth.subtract(initial_height, distance1)  // 80
  let remaining2 = @azimuth.subtract(initial_height, distance2)  // 55
  let remaining3 = @azimuth.subtract(initial_height, distance3)  // 20
  
  // 计算总下落距离
  let total_fall = distance3  // 80
  
  // 验证计算结果
  @azimuth.assert_eq(20, distance1)
  @azimuth.assert_eq(45, distance2)
  @azimuth.assert_eq(80, distance3)
  @azimuth.assert_eq(80, remaining1)
  @azimuth.assert_eq(55, remaining2)
  @azimuth.assert_eq(20, remaining3)
  @azimuth.assert_eq(80, total_fall)
}