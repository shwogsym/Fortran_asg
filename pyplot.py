#asg3とasg4のデータのプロット
#青がニュートン法、赤が二分法

import matplotlib.pyplot as plt

# ファイルパスの設定
file_paths = ['asg3/out.dat', 'asg4/data.dat']
colors = ['blue', 'red']

# データを格納するリスト
all_data = []

for file_path in file_paths:
    x_data = []
    y_data = []
    # ファイルを読み込み、DをEに置換してデータを抽出
    with open(file_path, 'r') as file:
        for line in file:
            # DをEに置換
            line = line.replace('D', 'E')
            # 空白で分割してデータを取得
            parts = line.split()
            # xとyのデータをリストに追加、parts[]が列を指定
            x_data.append(float(parts[0]))
            # y_data.append(float(parts[1]))
            y_data.append(abs(float(parts[1])))  # y_dataにabsを適用
    all_data.append((x_data, y_data))

#データのプロット
for (x_data, y_data), color in zip(all_data[::-1], colors[::-1]):  # 順序を逆にしてプロット
    plt.plot(x_data, y_data, marker='o', linestyle='-', color=color, markersize=4 )

plt.yscale('log')

#ラベル設定
plt.xlabel('Repetition times')  
plt.ylabel('error (difference from the true value)') #abs計算
# plt.ylabel('calculated value') #rel計算用
plt.title('Newton: brue, bisection: red')
plt.show()

