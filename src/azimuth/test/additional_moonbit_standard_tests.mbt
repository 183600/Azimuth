// 额外的标准 MoonBit 测试用例
// 这些测试用例提供了额外的测试覆盖，包括数学性质、实际应用场景和边界条件

test "negative_multiplication_commutativity" {
  // 测试负数乘法的交换律：a * b = b * a
  let a = -15
  let b = 7
  
  let result1 = @azimuth.multiply(a, b)
  let result2 = @azimuth.multiply(b, a)
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试两个负数相乘
  let c = -12
  let d = -8
  
  let result3 = @azimuth.multiply(c, d)
  let result4 = @azimuth.multiply(d, c)
  
  @azimuth.assert_eq(result3, result4)
}

test "large_number_operations_stability" {
  // 测试大数运算的稳定性
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = -1500000
  
  // 测试大数相加
  let sum1 = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, sum1)
  
  let sum2 = @azimuth.add(large_num1, large_num3)
  @azimuth.assert_eq(-500000, sum2)
  
  // 测试大数相乘
  let product1 = @azimuth.multiply(large_num1, 2)
  @azimuth.assert_eq(2000000, product1)
  
  let product2 = @azimuth.multiply(large_num3, 2)
  @azimuth.assert_eq(-3000000, product2)
}

test "string_special_characters_handling" {
  // 测试字符串包含各种特殊字符的处理
  // 测试包含HTML标签的字符串
  let html_name = "<div>User</div>"
  @azimuth.assert_eq_string("Hello, <div>User</div>!", @azimuth.greet(html_name))
  
  // 测试包含URL的字符串
  let url_name = "https://example.com/user"
  @azimuth.assert_eq_string("Hello, https://example.com/user!", @azimuth.greet(url_name))
  
  // 测试包含路径的字符串
  let path_name = "/home/user/documents"
  @azimuth.assert_eq_string("Hello, /home/user/documents!", @azimuth.greet(path_name))
  
  // 测试包含脚本的字符串
  let script_name = "function test() { return true; }"
  @azimuth.assert_eq_string("Hello, function test() { return true; }!", @azimuth.greet(script_name))
}

test "mathematical_associativity_properties" {
  // 测试数学运算的结合律性质
  // 加法结合律：(a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let x = 10
  let y = 20
  let z = 30
  
  let left_mult = @azimuth.multiply(@azimuth.multiply(x, y), z)
  let right_mult = @azimuth.multiply(x, @azimuth.multiply(y, z))
  
  @azimuth.assert_eq(left_mult, right_mult)
}

test "boundary_condition_combinations" {
  // 测试边界条件的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  let sum_max_min = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, sum_max_min)
  
  // 测试最大值与自身的加法（应该溢出）
  let sum_max_max = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, sum_max_max)
  
  // 测试最小值与自身的加法（应该溢出）
  let sum_min_min = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, sum_min_min)
  
  // 测试最大值与1的乘法
  let product_max_one = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, product_max_one)
  
  // 测试最小值与1的乘法
  let product_min_one = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, product_min_one)
}

test "bmi_calculation_scenario" {
  // 测试实际应用场景：BMI计算
  // BMI公式：BMI = 体重(kg) / (身高(m) * 身高(m))
  // 由于只有整数运算，我们使用近似值：BMI = 体重(kg) * 10000 / (身高(cm) * 身高(cm))
  
  let weight_kg = 70  // 体重70公斤
  let height_cm = 175  // 身高175厘米
  
  // 计算身高的平方：175 * 175 = 30625
  let height_squared = @azimuth.multiply(height_cm, height_cm)
  
  // 计算BMI分子：70 * 10000 = 700000
  let bmi_numerator = @azimuth.multiply(weight_kg, 10000)
  
  // 计算BMI：700000 / 30625 ≈ 22.86，整数除法为22
  let bmi = bmi_numerator / height_squared
  
  @azimuth.assert_eq(22, bmi)
  
  // 测试另一个例子
  let weight_kg2 = 85
  let height_cm2 = 180
  
  let height_squared2 = @azimuth.multiply(height_cm2, height_cm2)
  let bmi_numerator2 = @azimuth.multiply(weight_kg2, 10000)
  let bmi2 = bmi_numerator2 / height_squared2
  
  @azimuth.assert_eq(26, bmi2)
}

test "discount_calculation_scenario" {
  // 测试实际应用场景：折扣计算
  let original_price = 1000  // 原价1000元
  let discount_percentage = 20  // 折扣20%
  
  // 计算折扣金额：原价 * 折扣百分比 / 100
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  
  // 计算折后价格：原价 - 折扣金额
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(200, discount_amount)
  @azimuth.assert_eq(800, discounted_price)
  
  // 测试多件商品的总折扣
  let quantity = 3
  let total_original_price = @azimuth.multiply(original_price, quantity)
  let total_discount = @azimuth.multiply(discount_amount, quantity)
  let total_price = @azimuth.add(total_original_price, -total_discount)
  
  @azimuth.assert_eq(3000, total_original_price)
  @azimuth.assert_eq(600, total_discount)
  @azimuth.assert_eq(2400, total_price)
}

test "tax_calculation_scenario" {
  // 测试实际应用场景：税率计算
  let price = 500  // 商品价格500元
  let tax_rate = 8  // 税率8%
  
  // 计算税额：价格 * 税率 / 100
  let tax_amount = @azimuth.multiply(price, tax_rate) / 100
  
  // 计算含税总价：价格 + 税额
  let total_price_with_tax = @azimuth.add(price, tax_amount)
  
  @azimuth.assert_eq(40, tax_amount)
  @azimuth.assert_eq(540, total_price_with_tax)
  
  // 测试多级税率计算
  let price2 = 1200
  let basic_tax_rate = 8  // 基础税率8%
  let additional_tax_rate = 5  // 额外税率5%
  
  let basic_tax = @azimuth.multiply(price2, basic_tax_rate) / 100
  let additional_tax = @azimuth.multiply(price2, additional_tax_rate) / 100
  let total_tax = @azimuth.add(basic_tax, additional_tax)
  let final_price = @azimuth.add(price2, total_tax)
  
  @azimuth.assert_eq(96, basic_tax)
  @azimuth.assert_eq(60, additional_tax)
  @azimuth.assert_eq(156, total_tax)
  @azimuth.assert_eq(1356, final_price)
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式的计算
  // 计算：(a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  // 分步计算
  let sum_ab = @azimuth.add(a, b)  // 10 + 5 = 15
  let diff_cd = @azimuth.add(c, -d)  // 20 - 8 = 12
  let product_ab_cd = @azimuth.multiply(sum_ab, diff_cd)  // 15 * 12 = 180
  let product_ef = @azimuth.multiply(e, f)  // 3 * 7 = 21
  let result = @azimuth.add(product_ab_cd, product_ef)  // 180 + 21 = 201
  
  @azimuth.assert_eq(201, result)
  
  // 计算二次方程：ax² + bx + c，其中x=3, a=2, b=5, c=3
  let x = 3
  let quad_a = 2
  let quad_b = 5
  let quad_c = 3
  
  let x_squared = @azimuth.multiply(x, x)  // 3² = 9
  let ax_squared = @azimuth.multiply(quad_a, x_squared)  // 2 * 9 = 18
  let bx = @azimuth.multiply(quad_b, x)  // 5 * 3 = 15
  let quadratic_result = @azimuth.add(@azimuth.add(ax_squared, bx), quad_c)  // 18 + 15 + 3 = 36
  
  @azimuth.assert_eq(36, quadratic_result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续加法是否会导致溢出
  let result1 = @azimuth.add(@azimuth.add(max_val, 1), 1)
  @azimuth.assert_eq(max_val, result1)  // 应该保持最大值
  
  // 测试连续乘法是否会导致溢出
  let result2 = @azimuth.multiply(@azimuth.multiply(max_val, 2), 2)
  @azimuth.assert_eq(max_val, result2)  // 应该保持最大值
  
  // 测试负数连续运算
  let result3 = @azimuth.add(@azimuth.add(min_val, -1), -1)
  @azimuth.assert_eq(min_val, result3)  // 应该保持最小值
  
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, x!", @azimuth.greet("x"))
  
  // 测试极值与0的运算
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}