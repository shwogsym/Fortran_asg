import matplotlib.pyplot as plt

# ファイルパスの設定
file_path = 'asg4_file/outasg4.dat'

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
        # xとyのデータをリストに追加、parts[]が列を指定
        x_data.append(float(parts[0]))
        y_data.append(float(parts[1]))

#データのプロット
plt.plot( x_data, y_data, marker='o', linestyle='-')

#ラベル設定
plt.xlabel('Initial value')  
plt.ylabel('Convergence value')
plt.title('Data Plot')
plt.show()