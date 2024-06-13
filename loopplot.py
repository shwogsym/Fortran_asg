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
        x_data.append(float(parts[1]))
        y_data.append(float(parts[2]))

# x,yを入れ替えてデータの出力しているけど、元データを修正すれば変更の必要なし
plt.plot(y_data, x_data, marker='o', linestyle='-')
#plt.plot(x_data, y_data, marker='o', linestyle='-') 入れ替えしない場合

plt.xlabel('Initial value')  # X LabelをY Labelに変更
plt.ylabel('Convergence value')  # Y LabelをX Labelに変更
plt.title('Data Plot')
plt.show()
