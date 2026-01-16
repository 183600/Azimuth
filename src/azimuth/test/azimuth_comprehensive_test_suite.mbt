// Azimuth 综合测试套件
// 使用标准 MoonBit 测试语法，涵盖核心功能和实际应用场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 加法测试
  let sum_result = @azimuth.add(a, b)
  @azimuth.assert_eq(22, sum_result)
  
  // 乘法测试
  let product_result = @azimuth.multiply(a, b)
  @azimuth.assert_eq(105, product_result)
  
  // 复合运算：(a + b) * a
  let complex_result = @azimuth.multiply(@azimuth.add(a, b), a)
  @azimuth.assert_eq(330, complex_result)
}

test "negative_number_operations" {
  // 负数运算测试
  let negative = -10
  let positive = 25
  
  // 正负数相加
  let result1 = @azimuth.add(negative, positive)
  @azimuth.assert_eq(15, result1)
  
  // 负数乘法
  let result2 = @azimuth.multiply(negative, 3)
  @azimuth.assert_eq(-30, result2)
  
  // 两个负数相乘
  let result3 = @azimuth.multiply(negative, -4)
  @azimuth.assert_eq(40, result3)
}

test "zero_value_operations" {
  // 零值运算测试
  let zero = 0
  let number = 42
  
  // 加零测试
  let result1 = @azimuth.add(number, zero)
  @azimuth.assert_eq(number, result1)
  
  // 乘零测试
  let result2 = @azimuth.multiply(number, zero)
  @azimuth.assert_eq(0, result2)
  
  // 零乘零测试
  let result3 = @azimuth.multiply(zero, zero)
  @azimuth.assert_eq(0, result3)
}

test "greet_function_comprehensive" {
  // greet 函数综合测试
  // 基本问候测试
  let result1 = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result1)
  
  // 空字符串测试
  let result2 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result2)
  
  // 数字字符串测试
  let result3 = @azimuth.greet("2023")
  @azimuth.assert_eq_string("Hello, 2023!", result3)
  
  // 特殊字符测试
  let result4 = @azimuth.greet("@#$%")
  @azimuth.assert_eq_string("Hello, @#$%!", result4)
}

test "mathematical_identities" {
  // 数学恒等式测试
  let x = 12
  let y = 8
  
  // 加法恒等式：x + 0 = x
  let identity1 = @azimuth.add(x, 0)
  @azimuth.assert_eq(x, identity1)
  
  // 乘法恒等式：x * 1 = x
  let identity2 = @azimuth.multiply(x, 1)
  @azimuth.assert_eq(x, identity2)
  
  // 加法交换律：x + y = y + x
  let commutative1 = @azimuth.add(x, y)
  let commutative2 = @azimuth.add(y, x)
  @azimuth.assert_eq(commutative1, commutative2)
}

test "practical_business_calculations" {
  // 实际业务计算测试
  // 计算总价：单价 × 数量
  let unit_price = 50
  let quantity = 8
  let total_price = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(400, total_price)
  
  // 计算折扣：总价 × 折扣率
  let discount_rate = 10  // 10%
  let discount_amount = @azimuth.multiply(total_price, discount_rate) / 100
  @azimuth.assert_eq(40, discount_amount)
  
  // 计算最终价格：总价 - 折扣
  let final_price = @azimuth.add(total_price, -discount_amount)
  @azimuth.assert_eq(360, final_price)
}

test "scientific_calculations" {
  // 科学计算测试
  // 计算正方形面积：边长 × 边长
  let side_length = 6
  let square_area = @azimuth.multiply(side_length, side_length)
  @azimuth.assert_eq(36, square_area)
  
  // 计算长方形面积：长 × 宽
  let length = 12
  let width = 5
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, rectangle_area)
  
  // 计算立方体体积：边长 × 边长 × 边长
  let cube_side = 4
  let cube_volume = @azimuth.multiply(@azimuth.multiply(cube_side, cube_side), cube_side)
  @azimuth.assert_eq(64, cube_volume)
}

test "statistical_operations" {
  // 统计运算测试
  // 计算平均值
  let value1 = 20
  let value2 = 30
  let value3 = 40
  let value4 = 50
  let value5 = 60
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(value1, value2), value3), value4), value5)
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(40, average)
  
  // 计算加权平均
  let weight1 = 2
  let weight2 = 3
  let weight3 = 5
  let value_a = 10
  let value_b = 20
  let value_c = 30
  
  let weighted_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(value_a, weight1), @azimuth.multiply(value_b, weight2)), @azimuth.multiply(value_c, weight3))
  let total_weight = @azimuth.add(@azimuth.add(weight1, weight2), weight3)
  let weighted_average = weighted_sum / total_weight
  @azimuth.assert_eq(23, weighted_average)
}

test "algorithm_simulation" {
  // 算法模拟测试
  // 模拟累加算法：1 + 2 + 3 + ... + 10 = 55
  let sum1 = @azimuth.add(0, 1)
  let sum2 = @azimuth.add(sum1, 2)
  let sum3 = @azimuth.add(sum2, 3)
  let sum4 = @azimuth.add(sum3, 4)
  let sum5 = @azimuth.add(sum4, 5)
  let sum6 = @azimuth.add(sum5, 6)
  let sum7 = @azimuth.add(sum6, 7)
  let sum8 = @azimuth.add(sum7, 8)
  let sum9 = @azimuth.add(sum8, 9)
  let sum10 = @azimuth.add(sum9, 10)
  
  @azimuth.assert_eq(55, sum10)
  
  // 模拟阶乘计算：5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial_5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(5, 4), 3), 2), 1)
  @azimuth.assert_eq(120, factorial_5)
}

test "boundary_and_extreme_values" {
  // 边界和极值测试
  // 大数运算
  let large_num1 = 100000
  let large_num2 = 200000
  
  let large_sum = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(300000, large_sum)
  
  let large_product = @azimuth.multiply(large_num1, 2)
  @azimuth.assert_eq(200000, large_product)
  
  // 连续运算测试
  let chain_result = @azimuth.add(
    @azimuth.multiply(10, 5),
    @azimuth.add(@azimuth.multiply(20, 3), @azimuth.multiply(30, 2))
  )
  @azimuth.assert_eq(170, chain_result)
  
  // 嵌套运算测试
  let nested_result = @azimuth.multiply(
    @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5)),
    @azimuth.add(@azimuth.multiply(6, 7), @azimuth.multiply(8, 9))
  )
  @azimuth.assert_eq(4620, nested_result)
}