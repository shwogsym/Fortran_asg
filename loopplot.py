import matplotlib.pyplot as plt

# ファイルパスの設定
file_path = 'graphene.dat'

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
        x_data.append(float(parts[0]))  # 1列目のデータ
        y_data.append(float(parts[1]))  # 2列目のデータ

# データのプロット
plt.plot(x_data, y_data, marker='o', linestyle='', color='b')

# アスペクト比を1:1に設定
plt.axis('equal')

# ラベル設定
plt.xlabel('X axis')  
plt.ylabel('Y axis')
plt.title('Graphene structure')
plt.show()