// 标准 MoonBit 测试用例 - 为 Azimuth 项目新增
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "advanced_mathematical_series" {
  // 高等数学级数计算：计算等差数列和等比数列
  // 等差数列：3, 7, 11, 15, 19...
  let first_term = 3
  let common_diff = 4
  let n_terms = 5
  
  // 使用公式计算等差数列和：S = n/2 * (2a + (n-1)d)
  let arithmetic_sum = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  // 等比数列：2, 6, 18, 54, 162...
  let geo_first = 2
  let geo_ratio = 3
  let geo_terms = 4
  
  // 计算等比数列和：S = a * (r^n - 1) / (r - 1)
  let geo_sum = divide_with_ceil(
    multiply(geo_first, subtract(multiply(geo_ratio, geo_ratio), 1)),
    geo_ratio - 1
  )
  
  assert_eq(55, arithmetic_sum)  // 3 + 7 + 11 + 15 + 19 = 55
  assert_eq(80, geo_sum)  // 2 + 6 + 18 + 54 = 80
}

test "complex_business_inventory_system" {
  // 复杂业务库存系统测试
  let initial_inventory = 500
  let monthly_sales = 45
  let monthly_restock = 30
  let seasonal_multiplier = 2  // 季节性销售倍数
  let months_quarter = 3
  
  // 计算季度销售（考虑季节性因素）
  let quarterly_sales = multiply(monthly_sales, multiply(months_quarter, seasonal_multiplier))
  
  // 计算季度补货
  let quarterly_restock = multiply(monthly_restock, months_quarter)
  
  // 计算季度末库存
  let ending_inventory = add(subtract(initial_inventory, quarterly_sales), quarterly_restock)
  
  // 计算需要的存储空间（每箱装20个）
  let storage_boxes = divide_with_ceil(ending_inventory, 20)
  
  assert_eq(270, quarterly_sales)  // 45 * 3 * 2 = 270
  assert_eq(90, quarterly_restock)  // 30 * 3 = 90
  assert_eq(320, ending_inventory)  // 500 - 270 + 90 = 320
  assert_eq(16, storage_boxes)  // ceil(320/20) = 16
}

test "scientific_temperature_conversion" {
  // 科学温度转换计算
  let celsius_freezing = 0
  let celsius_boiling = 100
  let celsius_room = 22
  
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let fahrenheit_freezing = add(divide_with_ceil(multiply(celsius_freezing, 9), 5), 32)
  let fahrenheit_boiling = add(divide_with_ceil(multiply(celsius_boiling, 9), 5), 32)
  let fahrenheit_room = add(divide_with_ceil(multiply(celsius_room, 9), 5), 32)
  
  // 摄氏度转开尔文：K = C + 273
  let kelvin_freezing = add(celsius_freezing, 273)
  let kelvin_boiling = add(celsius_boiling, 273)
  let kelvin_room = add(celsius_room, 273)
  
  assert_eq(32, fahrenheit_freezing)  // 0°C = 32°F
  assert_eq(212, fahrenheit_boiling)  // 100°C = 212°F
  assert_eq(72, fahrenheit_room)  // 22°C = 71.6°F ≈ 72°F
  assert_eq(273, kelvin_freezing)  // 0°C = 273K
  assert_eq(373, kelvin_boiling)  // 100°C = 373K
  assert_eq(295, kelvin_room)  // 22°C = 295K
}

test "engineering_structural_calculations" {
  // 工程结构计算：桥梁承重计算
  let bridge_length = 50  // 米
  let bridge_width = 10   // 米
  let concrete_thickness = 2  // 米
  let concrete_density = 2400  // kg/m³
  let safety_factor = 3
  
  // 计算混凝土体积
  let concrete_volume = multiply(bridge_length, multiply(bridge_width, concrete_thickness))
  
  // 计算混凝土重量
  let concrete_weight = multiply(concrete_volume, concrete_density)
  
  // 计算设计承重（考虑安全系数）
  let design_load = multiply(concrete_weight, safety_factor)
  
  // 计算支撑点数量（每10米一个支撑点）
  let support_points = divide_with_ceil(bridge_length, 10)
  
  // 每个支撑点的承重
  let load_per_support = divide_with_ceil(design_load, support_points)
  
  assert_eq(1000, concrete_volume)  // 50 * 10 * 2 = 1000 m³
  assert_eq(2400000, concrete_weight)  // 1000 * 2400 = 2,400,000 kg
  assert_eq(7200000, design_load)  // 2,400,000 * 3 = 7,200,000 kg
  assert_eq(5, support_points)  // ceil(50/10) = 5
  assert_eq(1440000, load_per_support)  // ceil(7,200,000/5) = 1,440,000 kg
}

test "data_analysis_statistical_functions" {
  // 数据分析统计函数测试
  let data_points = [12, 15, 18, 21, 24, 27, 30]
  let n = 7
  
  // 计算总和
  let sum = add(add(add(add(add(add(data_points[0], data_points[1]), data_points[2]), data_points[3]), data_points[4]), data_points[5]), data_points[6])
  
  // 计算平均值（向上取整）
  let mean = divide_with_ceil(sum, n)
  
  // 计算中位数（已排序数组）
  let median = data_points[3]  // 第4个元素（0-based索引）
  
  // 计算范围
  let range = subtract(data_points[6], data_points[0])
  
  // 计算标准差（简化版本）
  let variance = divide_with_ceil(
    add(
      add(
        add(
          add(
            add(
              add(
                multiply(subtract(data_points[0], mean), subtract(data_points[0], mean)),
                multiply(subtract(data_points[1], mean), subtract(data_points[1], mean))
              ),
              multiply(subtract(data_points[2], mean), subtract(data_points[2], mean))
            ),
            multiply(subtract(data_points[3], mean), subtract(data_points[3], mean))
          ),
          multiply(subtract(data_points[4], mean), subtract(data_points[4], mean))
        ),
        multiply(subtract(data_points[5], mean), subtract(data_points[5], mean))
      ),
      multiply(subtract(data_points[6], mean), subtract(data_points[6], mean))
    ),
    n
  )
  
  assert_eq(147, sum)  // 12 + 15 + 18 + 21 + 24 + 27 + 30 = 147
  assert_eq(21, mean)  // ceil(147/7) = 21
  assert_eq(21, median)  // 中位数是21
  assert_eq(18, range)  // 30 - 12 = 18
  assert_eq(36, variance)  // 简化的方差计算
}

test "cryptographic_hash_simulation" {
  // 加密哈希模拟计算
  let input_value = 12345
  let prime_1 = 31
  let prime_2 = 37
  let modulo_value = 1000000
  
  // 模拟哈希算法：H = ((input * prime1) XOR (input * prime2)) % modulo
  let hash_component_1 = multiply(input_value, prime_1)
  let hash_component_2 = multiply(input_value, prime_2)
  
  // 简化的XOR操作（使用加法模拟）
  let xor_result = add(hash_component_1, hash_component_2)
  
  // 取模运算（使用除法模拟）
  let hash_result = xor_result - multiply(divide_with_ceil(xor_result, modulo_value), modulo_value)
  
  // 验证哈希值的唯一性
  let input_value_2 = 12346
  let hash_component_1_2 = multiply(input_value_2, prime_1)
  let hash_component_2_2 = multiply(input_value_2, prime_2)
  let xor_result_2 = add(hash_component_1_2, hash_component_2_2)
  let hash_result_2 = xor_result_2 - multiply(divide_with_ceil(xor_result_2, modulo_value), modulo_value)
  
  assert_eq(832320, hash_result)  // (12345 * 31) + (12345 * 37) = 832320
  assert_eq(835434, hash_result_2)  // (12346 * 31) + (12346 * 37) = 835434
  assert_true(hash_result != hash_result_2)  // 验证哈希值不同
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 5
  let compound_frequency = 12  // 月复利
  
  // 简化的复利计算（每年计算）
  let amount_year_1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let amount_year_2 = add(amount_year_1, divide_with_ceil(multiply(amount_year_1, annual_rate), 100))
  let amount_year_3 = add(amount_year_2, divide_with_ceil(multiply(amount_year_2, annual_rate), 100))
  let amount_year_4 = add(amount_year_3, divide_with_ceil(multiply(amount_year_3, annual_rate), 100))
  let amount_year_5 = add(amount_year_4, divide_with_ceil(multiply(amount_year_4, annual_rate), 100))
  
  // 计算总利息
  let total_interest = subtract(amount_year_5, principal)
  
  // 计算有效年利率
  let effective_rate = divide_with_ceil(multiply(total_interest, 100), principal)
  
  assert_eq(10800, amount_year_1)  // 10000 + 800 = 10800
  assert_eq(11664, amount_year_2)  // 10800 + 864 = 11664
  assert_eq(12597, amount_year_3)  // 11664 + 933 = 12597
  assert_eq(13605, amount_year_4)  // 12597 + 1008 = 13605
  assert_eq(14693, amount_year_5)  // 13605 + 1088 = 14693
  assert_eq(4693, total_interest)  // 14693 - 10000 = 4693
  assert_eq(47, effective_rate)  // ceil(4693 * 100 / 10000) = 47%
}

test "machine_learning_matrix_operations" {
  // 机器学习矩阵运算模拟
  // 2x2 矩阵乘法
  let a_11 = 2
  let a_12 = 3
  let a_21 = 1
  let a_22 = 4
  
  let b_11 = 5
  let b_12 = 6
  let b_21 = 7
  let b_22 = 8
  
  // 计算矩阵乘积 C = A * B
  let c_11 = add(multiply(a_11, b_11), multiply(a_12, b_21))
  let c_12 = add(multiply(a_11, b_12), multiply(a_12, b_22))
  let c_21 = add(multiply(a_21, b_11), multiply(a_22, b_21))
  let c_22 = add(multiply(a_21, b_12), multiply(a_22, b_22))
  
  // 计算矩阵的迹（对角线元素之和）
  let trace = add(c_11, c_22)
  
  // 计算行列式（简化版本）
  let determinant = subtract(multiply(c_11, c_22), multiply(c_12, c_21))
  
  assert_eq(31, c_11)  // 2*5 + 3*7 = 10 + 21 = 31
  assert_eq(36, c_12)  // 2*6 + 3*8 = 12 + 24 = 36
  assert_eq(33, c_21)  // 1*5 + 4*7 = 5 + 28 = 33
  assert_eq(38, c_22)  // 1*6 + 4*8 = 6 + 32 = 38
  assert_eq(69, trace)  // 31 + 38 = 69
  assert_eq(-110, determinant)  // 31*38 - 36*33 = 1178 - 1188 = -10
}

test "advanced_string_processing" {
  // 高级字符串处理测试
  let first_name = "张"
  let last_name = "三"
  let age = "25"
  let city = "北京"
  
  // 构建完整的个人信息字符串
  let full_name = first_name + last_name
  let personal_info = full_name + "，年龄" + age + "岁，来自" + city
  
  // 使用greet函数处理复杂字符串
  let formal_greeting = greet("尊敬的" + full_name + "先生")
  let casual_greeting = greet(full_name)
  let info_greeting = greet(personal_info)
  
  // 验证字符串长度和内容
  assert_eq_string("Hello, 尊敬的张三先生!", formal_greeting)
  assert_eq_string("Hello, 张三!", casual_greeting)
  assert_eq_string("Hello, 张三，年龄25岁，来自北京!", info_greeting)
  
  // 测试特殊字符和数字组合
  let special_chars = "!@#$%^&*()"
  let numbers = "1234567890"
  let mixed_string = special_chars + numbers + special_chars
  let mixed_greeting = greet(mixed_string)
  
  assert_eq_string("Hello, !@#$%^&*()1234567890!@#$%^&*()!", mixed_greeting)
}

test "physics_motion_calculations" {
  // 物理运动计算测试
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  let gravity = 10           // m/s²
  
  // 计算最终速度：v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  
  // 计算位移：s = ut + 0.5at²
  let displacement = add(multiply(initial_velocity, time), divide_with_ceil(multiply(acceleration, multiply(time, time)), 2))
  
  // 计算自由落体距离：h = 0.5gt²
  let free_fall_distance = divide_with_ceil(multiply(gravity, multiply(time, time)), 2)
  
  // 计算动能：KE = 0.5mv²（假设质量为2kg）
  let mass = 2
  let kinetic_energy = divide_with_ceil(multiply(mass, multiply(final_velocity, final_velocity)), 2)
  
  // 计算势能：PE = mgh（假设高度为10m）
  let height = 10
  let potential_energy = multiply(mass, multiply(gravity, height))
  
  assert_eq(20, final_velocity)  // 10 + 2*5 = 20 m/s
  assert_eq(75, displacement)    // 10*5 + 2*25/2 = 50 + 25 = 75 m
  assert_eq(125, free_fall_distance)  // 10*25/2 = 125 m
  assert_eq(400, kinetic_energy)  // 2*20²/2 = 400 J
  assert_eq(200, potential_energy)  // 2*10*10 = 200 J
}