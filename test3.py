import matplotlib.pyplot as plt

# ファイルパスを修正
file_path = 'dataloop.dat'  # 'dataloop.dat' から 'loop.dat' に変更

# データを格納するリスト
x_data = []
y_data = []

# ファイルを読み込み、DをEに置換してデータを抽出
with open(file_path, 'r') as file:
    for line in file:
        # DをEに置換
        line = line.replace('D', 'E')
        # 空白で分割してデータを取得
        parts = line.split()
        # xとyのデータをリストに追加
        x_data.append(float(parts[1]))
        y_data.append(float(parts[2]))

# データをプロット
plt.plot(x_data, y_data, marker='o', linestyle='-')
plt.xlabel('X Label')
plt.ylabel('Y Label')
plt.title('Data Plot')
plt.show()