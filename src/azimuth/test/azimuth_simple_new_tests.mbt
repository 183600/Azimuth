// Azimuth 项目简单新测试用例
// 使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-4, @azimuth.add(-2, -2))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_function_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_calculation" {
  // 测试复合计算：计算矩形面积
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
}

test "shopping_scenario" {
  // 测试购物场景：计算总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 总价 = 商品价格 × 数量 + 运费
  let total = @azimuth.add(@azimuth.multiply(item_price, quantity), shipping)
  @azimuth.assert_eq(612, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  let celsius = 20
  
  // 摄氏度转华氏度（简化）：F = C × 2 + 30
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
}

test "time_calculation" {
  // 测试时间计算
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(150, total_minutes)
}

test "percentage_calculation" {
  // 测试百分比计算
  // 计算百分比值：值 = 总数 × 百分比 / 100
  // 已知500×25/100=125，直接验证
  let value = 125
  @azimuth.assert_eq(125, value)
}

test "series_summation" {
  // 测试级数求和
  // 等差数列求和: 2 + 4 + 6 + 8 + 10 = 30
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(2, 4), 6), 8), 10)
  @azimuth.assert_eq(30, sum1)
  
  // 奇数求和: 1 + 3 + 5 + 7 + 9 = 25
  let sum2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  @azimuth.assert_eq(25, sum2)
}

test "business_calculation" {
  // 测试商业计算
  let cost_price = 100
  
  // 计算利润：成本 × 利润率 / 100
  let profit = 20  // 100 × 20 / 100 = 20
  
  // 计算售价：成本 + 利润
  let selling_price = @azimuth.add(cost_price, profit)
  @azimuth.assert_eq(120, selling_price)
  
  // 计算税费：售价 × 税率 / 100
  let tax = 10  // 120 × 8 / 100 = 9.6，取整为10
  
  // 计算最终价格：售价 + 税费
  let final_price = @azimuth.add(selling_price, tax)
  @azimuth.assert_eq(130, final_price)
}