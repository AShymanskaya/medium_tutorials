import React from 'react';

const EmotionalLandscape = () => {
  const emotions = [
    { name: 'Anger', ukr: 46, rus: 76.0, color: '#E60000' },
    { name: 'Sadness', ukr: 11, rus: 4, color: '#4B8BE8' },
    { name: 'Optimism', ukr: 10, rus: 6, color: '#BE5EBF' },
    { name: 'Joy', ukr: 9, rus: 3, color: '#FFD93D' },
    { name: 'Mix', ukr: 24, rus: 11, color: '#62BB47' }
  ];

  const maxPercentage = 80;
  const gapWidth = 100;
  const barWidth = 30;

  const getOpacity = (value1, value2) => value1 > value2 ? 0.9 : 0.5;

  const TextBox = ({ side, text }) => (
    <div className={`absolute ${side === 'left' ? 'left-[25%] -translate-x-1/2' : 'right-[25%] translate-x-1/2'} top-0 w-[480px]`}>
      <div className="bg-[#F5F8FA] p-3 rounded-lg border border-[#E1E8ED] text-center">
        <h4 className={`font-bold text-sm mb-2 ${side === 'left' ? 'text-[#1DA1F2]' : 'text-[#DC2626]'}`}>
          {`Pro-${side === 'left' ? 'Ukrainian' : 'Russian'} Posts`}
        </h4>
        <p className="text-[#657786] text-sm leading-relaxed">{text}</p>
      </div>
    </div>
  );

  const Bar = ({ emotion, side, value, width }) => (
    <div className={`absolute w-[42%] flex items-center ${side === 'left' ? 'right-0 justify-end' : ''}`}
         style={{ [side === 'left' ? 'right' : 'left']: `calc(50% + ${gapWidth/2}px)` }}>
      <div className="relative group flex items-center justify-end">
        {side === 'left' && (
          <span className="font-bold text-sm mr-4" style={{ color: emotion.color }}>
            {value}%
          </span>
        )}
        <div 
          className="h-16 rounded-lg transition-all duration-500 hover:scale-y-110"
          style={{
            width: `${width}vw`,
            backgroundColor: emotion.color,
            opacity: getOpacity(value, side === 'left' ? emotion.rus : emotion.ukr)
          }}
        />
        {side === 'right' && (
          <span className="font-bold text-sm ml-4" style={{ color: emotion.color }}>
            {value}%
          </span>
        )}
      </div>
    </div>
  );

  return (
    <div className="w-full max-w-6xl bg-white rounded-lg shadow-2xl mx-auto my-8">
      <div className="border-b border-[#E1E8ED] p-4 bg-gradient-to-b from-[#F5F8FA] to-white">
        <h2 className="text-3xl font-bold text-[#14171A] mb-2 text-center">
          Sentiments of War
        </h2>
        <p className="text-[#657786] text-center text-lg">
          Emotional expressions on Twitter during the first months of full-scale Russian invasion in Ukraine (Feb - Jun 2022)
        </p>
      </div>

      <div className="p-4">
        <div className="relative h-[500px] w-full flex">
          <TextBox 
            side="left" 
            text="A more distributed emotional spectrum, with anger tempered by mix of other sentiments. Higher presence of optimism suggests resilience."
          />
          <TextBox 
            side="right" 
            text="Dominated by intense anger, with minimal expression of other emotions. Shows a more uniform emotional response."
          />

          <div className="flex-1 relative mt-20">
            {emotions.map((emotion, index) => (
              <div key={emotion.name} className="absolute w-full" style={{ top: 30 + (index * 80) }}>
                <Bar 
                  emotion={emotion}
                  side="left"
                  value={emotion.ukr}
                  width={(emotion.ukr / maxPercentage) * barWidth}
                />
                
                <div className="absolute left-1/2 transform -translate-x-1/2 flex items-center justify-center h-16">
                  <div className="text-lg font-bold px-4 py-2 rounded-full whitespace-nowrap"
                       style={{ color: emotion.color }}>
                    {emotion.name}
                  </div>
                </div>

                <Bar 
                  emotion={emotion}
                  side="right"
                  value={emotion.rus}
                  width={(emotion.rus / maxPercentage) * barWidth}
                />
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
};

export default EmotionalLandscape;