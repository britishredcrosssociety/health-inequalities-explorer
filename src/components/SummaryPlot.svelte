<script>
    import { onMount } from 'svelte';
    import { LayerCake, Svg, Html } from 'layercake';
    import { scaleOrdinal } from 'd3-scale';
    import { group } from 'd3-array';

    import Beeswarm from './layercake/BeeswarmForce.svelte';
    import AxisX from './layercake/AxisX.svelte';

    let data;
    let groupedData;
    let xKey = 'scaled_1_1';
    let yKey = 'variable';
    let zKey = 'variable';
    let titleKey = 'label';
    let seriesColors = ['#000'];

    const r = 6;

    onMount(async () => {
        const response = await fetch('https://humaniverse.r-universe.dev/hiedata/data/england_icb_summary_metrics/json');
        data = await response.json();

        groupedData = Array.from(group(data, d => d.variable), ([key, values]) => ({
            variable: key,
            values: values
        }));
    });
</script>

{#if groupedData}
    <div class="chart-wrapper">
        {#each groupedData as group, i}
            <div class="variable-group">
                <h3>{group.variable}</h3>
                
                <div class="beeswarm-container">
                    <!-- Only show labels above first chart -->
                    {#if i === 0}
                        <div class="labels">
                            <span class="label left">← Worse than average</span>
                            <span class="label right">Better than average →</span>
                        </div>
                    {/if}
                    <LayerCake
                        padding={{ top: 10, bottom: 20 }}
                        x={xKey}
                        z={zKey}
                        xDomain={[-1, 1]}
                        zScale={scaleOrdinal()}
                        zRange={seriesColors}
                        let:width
                    >
                        <Svg>
                            <Beeswarm
                                data={group.values}
                                r={width < 400 ? r / 1.25 : r}
                                strokeWidth={5}
                                xStrength={0.95}
                                yStrength={0.075}
                                getTitle={d => d[titleKey]}
                            />
                            <AxisX 
                                gridlines={true}
                                ticks={[-1, -0.5, 0, 0.5, 1]}
                                tickMarks={i === groupedData.length - 1}
                                baseline={i === groupedData.length - 1}
                                showText={i === groupedData.length - 1}
                            />
                        </Svg>
                    </LayerCake>
                </div>
            </div>
        {/each}
    </div>
{/if}
  
<style>
    /*
      The wrapper div needs to have an explicit width and height in CSS.
      It can also be a flexbox child or CSS grid element.
      The point being it needs dimensions since the <LayerCake> element will
      expand to fill it.
    */
    .chart-container {
      width: 100%;
      height: 100px;
    }
    .chart-wrapper {
        width: 100%;
    }
    .variable-group {
        margin-bottom: 1rem;
    }
    .variable-group h3 {
        margin: 0 0 0.5rem 0;
        font-size: 1rem;
        color: #333;
    }
    .beeswarm-container {
        height: 80px;
    }
    :global(.beeswarm-container .layercake-container) {
        height: 100%;
    }
    
    @media (min-width: 768px) {
        .variable-group {
            display: flex;
            align-items: center;
            gap: 1rem;
        }
        .variable-group h3 {
            flex: 0 0 200px;
            margin: 0;
            text-align: right;
        }
        .beeswarm-container {
            flex: 1;
        }
    }
    .labels {
        display: flex;
        justify-content: space-between;
        padding: 0 1rem;
        margin-bottom: 0.5rem;
        font-size: 0.8rem;
        color: #666;
    }
    .label {
        user-select: none;
    }
</style>