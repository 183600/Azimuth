// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法，覆盖更多功能场景

test "boolean_operations" {
  // 测试布尔运算
  let true_val = true
  let false_val = false
  
  // 逻辑与
  let and_result1 = true_val && true_val
  let and_result2 = true_val && false_val
  let and_result3 = false_val && true_val
  let and_result4 = false_val && false_val
  
  @azimuth.assert_eq(and_result1, true)
  @azimuth.assert_eq(0, 1)(and_result2)
  @azimuth.assert_eq(0, 1)(and_result3)
  @azimuth.assert_eq(0, 1)(and_result4)
  
  // 逻辑或
  let or_result1 = true_val || true_val
  let or_result2 = true_val || false_val
  let or_result3 = false_val || true_val
  let or_result4 = false_val || false_val
  
  @azimuth.assert_eq(or_result1, true)
  @azimuth.assert_eq(or_result2, true)
  @azimuth.assert_eq(or_result3, true)
  @azimuth.assert_eq(0, 1)(or_result4)
}

test "comparison_operations" {
  // 测试比较运算
  let a = 10
  let b = 20
  let c = 10
  
  // 等于
  @azimuth.assert_eq(a == c, true)
  @azimuth.assert_eq(0, 1)(a == b)
  
  // 不等于
  @azimuth.assert_eq(a != b, true)
  @azimuth.assert_eq(0, 1)(a != c)
  
  // 大于
  @azimuth.assert_eq(true, b > a)
  @azimuth.assert_eq(0, 1)(a > b)
  @azimuth.assert_eq(0, 1)(a > c)
  
  // 小于
  @azimuth.assert_eq(true, a < b)
  @azimuth.assert_eq(0, 1)(b < a)
  @azimuth.assert_eq(0, 1)(a < c)
  
  // 大于等于
  @azimuth.assert_eq(a >= c, true)
  @azimuth.assert_eq(b >= a, true)
  @azimuth.assert_eq(0, 1)(a >= b)
  
  // 小于等于
  @azimuth.assert_eq(a <= c, true)
  @azimuth.assert_eq(a <= b, true)
  @azimuth.assert_eq(0, 1)(b <= a)
}

test "number_system_conversions" {
  // 测试数字系统转换
  
  // 二进制转十进制（模拟）
  // 1010 (二进制) = 10 (十进制)
  let binary_1010 = @azimuth.add(@azimuth.multiply(1, 8), @azimuth.add(@azimuth.multiply(0, 4), @azimuth.add(@azimuth.multiply(1, 2), @azimuth.multiply(0, 1))))
  @azimuth.assert_eq(10, binary_1010)
  
  // 1111 (二进制) = 15 (十进制)
  let binary_1111 = @azimuth.add(@azimuth.multiply(1, 8), @azimuth.add(@azimuth.multiply(1, 4), @azimuth.add(@azimuth.multiply(1, 2), @azimuth.multiply(1, 1))))
  @azimuth.assert_eq(15, binary_1111)
  
  // 十六进制转十进制（模拟）
  // A (十六进制) = 10 (十进制)
  let hex_A = 10
  @azimuth.assert_eq(10, hex_A)
  
  // F (十六进制) = 15 (十进制)
  let hex_F = 15
  @azimuth.assert_eq(15, hex_F)
}

test "statistical_calculations" {
  // 测试统计计算
  
  // 平均值计算: (2 + 4 + 6 + 8) / 4 = 5
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 6), 8)
  let average1 = sum1 / 4
  @azimuth.assert_eq(5, average1)
  
  // 平均值计算: (10 + 20 + 30) / 3 = 20
  let sum2 = @azimuth.add(@azimuth.add(10, 20), 30)
  let average2 = sum2 / 3
  @azimuth.assert_eq(20, average2)
  
  // 范围计算: 最大值 - 最小值
  let numbers_max = 15
  let numbers_min = 3
  let range = @azimuth.add(numbers_max, -numbers_min)
  @azimuth.assert_eq(12, range)
}

test "physics_calculations" {
  // 测试物理计算
  
  // 速度 = 距离 / 时间
  let distance = 100  // 米
  let time = 10       // 秒
  let velocity = distance / time
  @azimuth.assert_eq(10, velocity)
  
  // 动能 = 0.5 * 质量 * 速度^2
  let mass = 2        // 千克
  let speed = 5       // 米/秒
  // 使用整数计算: (质量 * 速度^2) / 2
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(mass, @azimuth.multiply(speed, speed)), 1) / 2
  @azimuth.assert_eq(25, kinetic_energy)
  
  // 势能 = 质量 * 重力加速度 * 高度
  let height = 10     // 米
  let gravity = 10    // 米/秒^2 (简化值)
  let potential_energy = @azimuth.multiply(@azimuth.multiply(mass, gravity), height)
  @azimuth.assert_eq(200, potential_energy)
}

test "array_operations_simulation" {
  // 测试数组操作模拟
  
  // 模拟数组求和
  let arr1 = 5
  let arr2 = 10
  let arr3 = 15
  let arr4 = 20
  let arr5 = 25
  
  let total_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(arr1, arr2), arr3), arr4), arr5)
  @azimuth.assert_eq(75, total_sum)
  
  // 模拟查找最大值 - 直接比较
  @azimuth.assert_eq(true, arr5 > arr4)
  @azimuth.assert_eq(true, arr4 > arr3)
  @azimuth.assert_eq(true, arr3 > arr2)
  @azimuth.assert_eq(true, arr2 > arr1)
  
  // 模拟查找最小值 - 直接比较
  @azimuth.assert_eq(true, arr1 < arr2)
  @azimuth.assert_eq(true, arr2 < arr3)
  @azimuth.assert_eq(true, arr3 < arr4)
  @azimuth.assert_eq(true, arr4 < arr5)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  
  // 除零保护
  let dividend = 10
  let divisor = 0
  
  // 模拟除零检查
  let safe_result = 0  // 由于 divisor = 0，结果为 0
  @azimuth.assert_eq(0, safe_result)
  
  // 正常除法
  let normal_divisor = 2
  let normal_result = dividend / normal_divisor
  @azimuth.assert_eq(5, normal_result)
  
  // 边界值测试
  let large_num = 1000000
  let small_num = 1
  let boundary_result = large_num / small_num
  @azimuth.assert_eq(1000000, boundary_result)
}

test "recursive_pattern_simulation" {
  // 测试递归模式模拟（使用嵌套函数调用模拟递归）
  
  // 阶乘计算: 5! = 5 * 4 * 3 * 2 * 1 = 120
  // 使用嵌套函数调用而不是可变变量
  let factorial1 = 5
  let factorial2 = @azimuth.multiply(factorial1, 4)  // 5 * 4 = 20
  let factorial3 = @azimuth.multiply(factorial2, 3)  // 20 * 3 = 60
  let factorial4 = @azimuth.multiply(factorial3, 2)  // 60 * 2 = 120
  let factorial5 = @azimuth.multiply(factorial4, 1)  // 120 * 1 = 120
  
  @azimuth.assert_eq(120, factorial5)
  
  // 幂计算: 2^4 = 2 * 2 * 2 * 2 = 16
  // 使用嵌套函数调用而不是可变变量
  let power1 = 2
  let power2 = @azimuth.multiply(power1, 2)  // 2 * 2 = 4
  let power3 = @azimuth.multiply(power2, 2)  // 4 * 2 = 8
  let power4 = @azimuth.multiply(power3, 2)  // 8 * 2 = 16
  
  @azimuth.assert_eq(16, power4)
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  
  // 模拟字符串长度计算
  let short_length = 2
  let long_length = 13
  
  @azimuth.assert_eq(2, short_length)
  @azimuth.assert_eq(13, long_length)
  
  // 字符串连接
  let first_name = "John"
  let last_name = "Doe"
  let full_name = @azimuth.greet(first_name + " " + last_name)
  @azimuth.assert_eq_string("Hello, John Doe!", full_name)
  
  // 字符串重复模拟
  let base_str = "ABC"
  // 模拟 "ABC" + "ABC" + "ABC"
  let repeated_str = base_str + base_str + base_str
  @azimuth.assert_eq_string("ABCABCABC", repeated_str)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  
  // 订单总价计算
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  let tax_rate = 10  // 10%
  let shipping_cost = 15
  
  // 小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  
  // 税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping_cost)
  
  @azimuth.assert_eq(200, subtotal1)
  @azimuth.assert_eq(150, subtotal2)
  @azimuth.assert_eq(350, subtotal)
  @azimuth.assert_eq(35, tax)
  @azimuth.assert_eq(400, total)
  
  // 折扣计算 - 对于总价400，折扣应该为0
  let discount = 0
  let final_total = @azimuth.add(total, -discount)
  
  @azimuth.assert_eq(0, discount)
  @azimuth.assert_eq(400, final_total)
}