import matplotlib.pyplot as plt

# ファイルパスの設定
file_path = 'loopdata.dat'

# データを格納するリスト
x_data = []
y_data = []
z_data = []  # 3列目のデータ用リスト

# ファイルを読み込み、DをEに置換してデータを抽出
with open(file_path, 'r') as file:
    for line in file:
        # DをEに置換
        line = line.replace('D', 'E')
        # 空白で分割してデータを取得
        parts = line.split()
        # x, y, zのデータをリストに追加
        x_data.append(float(parts[0]))  # 1列目のデータ
        y_data.append(float(parts[1]))  # 2列目のデータ
        z_data.append(float(parts[2]))  # 3列目のデータ

# プロットする列を選択
#初期値に対する収束先を見る用
# x_col = z_data  
# y_col = y_data  

#初期値に対する、反復回数を見るよう
x_col = z_data  # x軸に使用するデータ
y_col = x_data  # y軸に使用するデータ

# データのプロット
plt.plot(x_col, y_col, marker='o', linestyle='', color='b', markersize = 3)

# アスペクト比を1:1に設定
# plt.axis('equal')

# ラベル設定
plt.xlabel('Initial value')  
# plt.ylabel('Convergence value')
plt.ylabel('Number of repetitions')  


plt.title('x^3 - x = 0')
plt.show()