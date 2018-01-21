# Wilson-disease
# Прогноз неврологических проявлений при болезни Вильсона-Коновалова
## Понимание задачи
### Цель

Выявить влияние полиморфизма генов свёртывающей системы крови <br> 
на риск неврологических проявлений (осложнений) при болезни Вильсона-Коновалова (БВК).

### Ценность
1. Уточнение прогноза риска неврологических осложнений при БВК.
2. Проверка гипотезы о влиянии генетических особенностей свёртывающей системы крови на риск неврологических осложнений при БВК.
3. Прогноз последствий в случае, если пациент не станет лечиться.

### Критерии качества
1. Важность отдельных признаков: корреляция с целевым показателем и её статистическая значимость.
2. Метрики качества предсказательной модели: <br> - вероятностная метрика риска (log_loss) - удобно для оптимизации <br> - качество ранжирования пациентов по риску (ROC AUC) - выявление групп пациентов с высоким и низким риском (участки на ROC кривой).
3. Вклад генетических анализов в метрики качества и достоверность этого вклада.

## Понимание предметной области
### Описание болезни Вильсона-Коновалова

Болезнь [Вильсона-Коновалова](https://ru.wikipedia.org/wiki/Болезнь_Вильсона_—_Коновалова) (БВК) или [Wilson's disease](https://en.wikipedia.org/wiki/Wilson%27s_disease) - врождённое генетически обусловленное заболевание. Проявляется в нарушении транспорта (вывода) меди из клеток, прежде всего, печени. Клетки печени, головного мозга и других оранов содержат белки, связывающие медь. Накопление меди в клетках печени может приводить к их повреждению, воспалению печени, а в последствие - к циррозу печени. 

Когда из печени в кровоток выделяется свободная медь, она оседает в разных органах: головном мозге, глазах, почках и др. [Симптомы](http://www.niddk.nih.gov/health-information/liver-disease/wilson-disease)  болезни зависят от того, какие органы оказались сильно поражены из-за избытка меди: печень, центральная нервная система, почки, ...

БВК связана с недостатком в печени белка [ATP7B](https://en.wikipedia.org/wiki/Wilson_disease_protein),  транспортирующего (выводящего) медь из клеток. Недостаток белка вызывается мутациями гена [ATP7B](https://www.niddk.nih.gov/health-information/liver-disease/wilson-disease). БВК возникает с вероятностью 1/4 при наличии мутации этого гена у обоих родителей. БВК встречается примерно у 30 из 1 млн человек, одинаково часто у мужчин и женщин. Как правило, симптомы впервые проявляются в возрасте от 5 до 35 лет. 

Прогноз [успешности](https://www.smed.ru/guides/43608)  лечения зависит от раннего начала лечения, от возраста больного, наличия развившегося поражения центральной нервной системы и других органов. Развившееся тяжёлое поражение печени или центральной нервной системы может уменьшиться в результате лечения, но часто бывает [необратимо](https://en.wikipedia.org/wiki/Wilson%27s_disease). 

### Риски при хронических заболеваниях печени, связанные с нарушениями свёртывающей системы крови 

Генетические нарушения свёртывающей системы крови могут приводить к избыточной свёртываемости крови – тромбофилии.
Это создаёт риски нарушения кровотока в сосудах (венозных и артериальных тромбозов), хронического нарушения текучести крови. 
Возрастают риски недостаточного кровоснабжения (ишемии) внутренних органов
и прогрессирующего разрастания соединительной ткани (фиброза) при хронических заболеваниях печени \[1\], \[2\].

Генетически детерминированная склонность организма к развитию тромбозов или внутрисосудистого
свертывания крови связана с наличием вариабельных участков ДНК генов белков свертывающей системы крови. 
Известна связь генетических нарушений свёртывающей системы крови с течением хронических заболеваний печени \[1\], \[2\].

Тромбофилия могла бы быть фактором риска развития неврологических осложнений и при БВК,
из-за возможного повреждения и кровоточивости сосудов в мозге. Вопрос недостаточно изучен.
Одна из целей работы - оценить этот риск.

\[1\] Роль аллельных вариантов генов свертывания крови в скорости прогрессирования фиброза при хронических заболеваниях печени / Е. Е. Старостина, С. В. Фастовец, Л. М. Самоходская, Т.П. Розина, Т.М. Игнатова, Т.Н. Краснова,Н.А. Мухин // Фарматека. — 2015. — № 20. — С. 17–23. [Ссылка](https://istina.msu.ru/publications/article/14797638/), [pdf](https://istina.msu.ru/download/17844477/1ecRKy:3b-LtwXVmcGZeOqN5q_5uhTLNEo/)

\[2\] Клиническое значение полиморфизма генов гемостаза и тромбоцитарных рецепторов у больных с хроническим гепатитом C.  / Е. Е. Старостина // Автореферат диссертации на соискание степени кандидата медицинских наук // 2016 [Ссылка](http://dissovet.rudn.ru/web-local/prep/rj/index.php?id=17&mod=dis&dis_id=1067), [pdf](http://dissovet.rudn.ru/web-local/prep/rj/dis/download.php?file=9821d86b42fb8f20dfb9c363c2b56b8211633), [pdf диссертации](http://dissovet.rudn.ru/web-local/prep/rj/dis/download.php?file=0efee378a77ab883efeeacdd715447aa31023)

## План исследования
1. Изучить первичные данные, выявить в них наиболее яркие корреляции, закономерности.
2. Построить предсказательные модели для риска неврологических проявлений болезни [Вильсона-Коновалова](https://ru.wikipedia.org/wiki/Болезнь_Вильсона_—_Коновалова) (БВК), опираясь на 
    * Историю заболевания и симпотомы пациента и его родственников (если доступно).
    * Особенности генов (минорные аллели), регулирующих свёртываемость крови. 
3. Выяснить, насколько особенности генов (минорные аллели), регулирующих свёртывающую систему крови, <br> позволяют уточнить прогноз тяжести неврологических проявлений при болезни Вильсона-Коновалова (БВК).

### Оперативные планы: backlog
1. Создать новые признаки для результатов генетических анализов - сделано
2. Из признаков рост, вес, BMI оставить один: BMI
3. Оценить качество предсказания
4. Сравнить качество предсказания с генетическими признаками и без них
5. Рефакторинг:
    * отделить подготовку данных от исследования: подготовить несколько форматов.
    * организовать проект в виде вычислительных "экспериментрв".

## Описание данных
### Содержание первичных данных
Данные относятся к пациентам с подтверждённой болезнью Вильсона-Коновалова.
В состав данных о пациенте входят
* **Целевой показатель**: наличие у пациента неврологических проявлений (осложнений) при болезни Вильсона-Коновалова.
* **Первичное описание пациента**: пол, рост, вес, индекс массы тела. 
* **Семейная информация**: Наличие близких родственных связей между пациентами (братья-сестры). <br> Наихудший целевой показатель у близких родственников (наличие у кого-то из них неврологических осложнений при БВК).
* **Возраст**, при котором впервые выявлено поражение печени или подозрение на болезнь Вильсона-Коновалова.
* **Характер поражения печени**: <br> наличие или отсутствие цирроза печени, <br> индекс [Чайлда-Пью](https://ru.wikipedia.org/wiki/Классификация_Чайлда_—_Пью) тяжести цирроза, <br> продвинутая стадия поражения печени, <br> активность процесса поражения печени (наличие активного воспалительно процесса, ...).
* **Дебют заболевания** - список поражённых органов, причина первичного обследования:  1. печеночная патология, 2. неврологическая, 3. почечная, 4. эндокринная, 5. сибсы (выявлена БВК у брата или сестры), 6. васкулит, 7. гемолитич. анемия, 8. разрыв селезенки. 9. обследование по другой патологии.
* **Кольцо [Кайзера-Флейшера](https://ru.wikipedia.org/wiki/Кольца_Кайзера_—_Флейшера)**: изменение цвета края радужной оболочки глаза (признак отложения меди в глазах).
* **Результаты генетического исследования**: аллельные варианты генов свёртывающей системы крови - какие нуклеотиды стоят на определённой позиции в гене для каждой из парных хромосом. <br> Список генов: [F2](https://ghr.nlm.nih.gov/gene/F2), [F5](https://ghr.nlm.nih.gov/gene/F5), F7, F13, ITGA2, ITGB3, [PAI_1](https://en.wikipedia.org/wiki/Plasminogen_activator_inhibitor-1), FGB, MTHFR (аллели в позициях 677 и 1298).

#### Неинформативные факторы, напрямую связанные с целевым показателем
Необходимо исключить из модели факторы, которые напрямую связаны с целевым показателем, но не несут предсказательной ценности при оценке информативности генетического анализа:

* **Семейная информация** - порядковый номер семьи в списке пациентов: **family_id**.
* **Дебют заболевания** - причина первичного обследования: **2. неврологическая**.


### Интерпретация генетических анализов
Результаты генетических анализов содержат внутреннюю структуру и, кроме того, могут быть обогащены интерпретацией - внешним знанием. И то, и другое можно использовать при моделировании.

**Disclaimer**: Интерпретация результатов генетического исследования в медицинских целях должна проводиться врачом в комплексе с другими генетическими, анамнестическими, клиническими и лабораторными данными. Здесь результаты генетических анализов интерпретируются чисто формально, исключительно с исследовательской целью.

Генетические анализы выявляют, какая аминокислота (или последовательность аминокислот) расположена в определённых участках хромосом. Обозначение результатов анализа - для обеих парных хромосом:
* \[Ген\]: \[позиция\] G>T - поверка, что в хромосоме в \[гене\] белка на определённой \[позиции\] вместо аминокислоты G (которая там встречается обычно) находится аминокислота T. <br> Результаты анализа - для каждой из парных хромосом: GG, GT или TT.
* \[Ген\]: \[позиция\] 5G>4G - проверка числа повторений аминокислоты G на определённой позиции в хромосоме. <br> Результа анализа - для каждой из парных хромосом: 5G5G, 5G4G или 4G4G.

Возможные результаты каждого генетического анализа упорядочены: 
1. "обычная" конфигурация аминокислот в обеих хромосомах, 
2. наличие аллельного варианта только в одной из хромосом,
3. наличие аллельного варианта в обеих хромосомах.

Базы знаний для медицинских анализов, которые ведут коммерческие лаборатории, [helix.ru/kb](https://helix.ru/kb) и [invitro.ru/analizes/for-doctors/](https://www.invitro.ru/analizes/for-doctors/), позволяют приписать факторы риска различным аллельным вариантам. Клинические проявления могут возникать при аллельном варианте в одной хромососме, либо в обеих хромосомах, либо только в сочетании с другими мутациями.

<html>
 <table style="width:100%">
  <tr>
    <th width="14%">Ген</th>
    <th width="10%"><p>Возможные аллельные варианты</p></th>
    <th width="7%"><p>Важные аллельные варианты</p></th>
    <th width="6%"><p> Фактор риска: +1, -1</p></th>
    <th width="56%" align="center">Описание</th>
    <th width="7%">Ссылка</th>
  </tr>
  <tr>
    <td>F2: 20210 G>A</td>
    <td>GG, GA, AA</td>
    <td>GA, AA</td>
    <td>+1</td>
    <td><p><font size="1">Ген протромбина (F2): 20210 G>A. Аллельные варианты  G/A (гетерозиготный) и A/A (гомозиготный) приводят повышенной экспрессии гена - синтезу большего количества белка - увеличивают риск тромбозов.</font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-031">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>F5: 1691 G>A</td>
    <td>GG, GA, AA</td>
    <td>GA, AA</td>
    <td>+1</td>
    <td><p><font size="1">Ген коагуляционного фактора V - мутация Лейдена - (F5): 1691 G>A. Аллельные варианты  G/A (гетерозиготный) и A/A (гомозиготный) придают устойчивость активной форме проакцелерина, увеличивают риск тромбозов.  </font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-030">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>F7: 10976 G>A</td>
    <td>GG, GA, AA</td>
    <td>GA, AA</td>
    <td>-1</td>
    <td><p><font size="1">Ген коагуляционного фактора VII (F7): 10976 G>A. Аллельные варианты G/A и А/A гена F7 приводят к понижению экспрессии гена и снижению уровня фактора VII в крови. Фактор снижения риска тромбозов и инфаркта миокарда.</font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-026">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>F13: 103 G>T</td>
    <td> GG,  GT, TT</td>
    <td>TT</td>
    <td>-1</td>
    <td><p><font size="1">  Ген коагуляционного фактора ХIII (F13): 103 G>T. Аллельный вариант T/T рассматриваются как фактор снижения риска тромбозов и инфаркта миокарда – активность белка F13 значительно снижена.
</font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-025">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>ITGA2: 807 C>T</td>
    <td>CC, CТ, ТТ</td>
    <td>CТ (?), ТТ</td>
    <td>+1</td>
    <td><p><font size="1">Ген интегрина альфа-2 (ITGA2): 807 С>T. Аллельный вариант T/T рассматриваются как фактор повышения риска тромбозов и инфаркта миокарда. Скорость адгезии тромбоцитов повышена, по сравнению с генотипами С/С и С/Т</font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-021">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>ITGB3: 1565 T>C</td>
    <td>TT, TC, CC</td>
    <td>TC, CC</td>
    <td>+1</td>
    <td><p><font size="1">Ген интегрина бета-3 (ITGB3): 1565 T>C. Аллельный варианты Т/С и С/С связаны с повышенной адгезией тромбоцитов. Рассматриваются как факторы повышения риска тромбозов и инфаркта миокарда.</font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-034">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>PAI-1: 675 5G>4G</td>
    <td>5G5G, 5G4G, 4G4G</td>
    <td>4G4G</td>
    <td>+1</td>
    <td><p><font size="1">Ген ингибитора активатора плазминогена (PAI-1): 675 5G>4G. Аллельные варианты 5G и 4G влияют на количество производимого белка. Аллельный вариант 4G/4G (гомозигота) ассоциирован с повышением риска  тромбообразования. В комбинации с F2: G/A или A/A - ещё больший риск тромбообразования.</font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-031">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>FGB: -455 G>A</td>
    <td>GG, GA, AA</td>
    <td>GA, AA</td>
    <td>+1</td>
    <td><p><font size="1">Ген фибриногена (FGB): -455 G>A. Аллельные варианты  G/A и A/A увеличивают риск тромбозов. <br> Повышенный уровень фибриногена в крови, вызванный присутствием в генотипе аллеля А, может приводить к атеросклеротическим изменениям в сосудах головного мозга, вызывая тем самым инсульты. Причем у гомозигот по аллелю А (генотип А/А) чаще наблюдается повреждение сосудов, по сравнению с гетерозиготами (генотип G/A).</font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-023">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>MTHFR: 677 С>T</td>
    <td>СС, СT, TT</td>
    <td>TT</td>
    <td>+1</td>
    <td><p><font size="1">Ген метилентетрагидрофолатредуктазы MTHFR: мутация 677 С>T. Тромбоциты носителей аллеля С имеют повышенную склонность к агрегации, что может являться причиной увеличения риска тромбообразования. </font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-031">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
  <tr>
    <td>MTHFR 1298</td>
    <td>AA, AC, CC</td>
    <td>AC, CC - в сочетании с MTHFR: 677 TT</td>
    <td>+1</td>
    <td><p><font size="1">Ген метилентетрагидрофолатредуктазы MTHFR: мутация 1298 A>C.  </font></p></th>
    <td><p><a href="https://helix.ru/kb/item/18-008">helix.ru</a>, <a href="https://www.invitro.ru/analizes/for-doctors/841/21927/">invitro.ru</a></p></td>
  </tr>
</table> 
</html>
