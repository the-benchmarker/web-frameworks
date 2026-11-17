require 'json'
require 'kramdown'

class MarkdownTableConverter < Kramdown::Converter::Kramdown
  def convert_table(elt, opts)
    opts[:alignment] = elt.options[:alignment]
    inner(elt, opts)
  end

  def convert_thead(elt, opts)
    rows = inner(elt, opts)
    markers = opts[:alignment].map do |alignment|
      case alignment
      when :left
        ':---'
      when :center
        ':---:'
      when :right
        '---:'
      else
        '---'
      end
    end

    "#{rows}| #{markers.join(' | ')} |\n"
  end

  def convert_tbody(elt, opts)
    inner(elt, opts)
  end

  def convert_tr(elt, opts)
    "| #{elt.children.map { |child| convert(child, opts) }.join(' | ')} |\n"
  end

  def convert_td(elt, opts)
    inner(elt, opts)
  end
end

def kramdown_text(value)
  Kramdown::Element.new(:text, value.to_s)
end

def kramdown_cell(value)
  cell = Kramdown::Element.new(:td)
  cell.children << kramdown_text(value)
  cell
end

def kramdown_row(values)
  row = Kramdown::Element.new(:tr)
  values.each { |value| row.children << kramdown_cell(value) }
  row
end

def kramdown_table(headers, rows)
  table = Kramdown::Element.new(:table, nil, nil, alignment: %i[left left right right right])
  thead = Kramdown::Element.new(:thead)
  tbody = Kramdown::Element.new(:tbody)

  thead.children << kramdown_row(headers)
  rows.each { |row| tbody.children << kramdown_row(row) }

  table.children << thead
  table.children << tbody
  table
end

namespace :export do
  task :md do
    data = JSON.parse(File.read('data.json'))

    concurrency = 256
    frameworks = data.fetch('frameworks')
    metrics_by_framework = Hash.new { |hash, key| hash[key] = {} }

    data.fetch('metrics').each do |metric|
      next unless metric.fetch('level') == concurrency

      metrics_by_framework[metric.fetch('framework_id')][metric.fetch('label')] = metric.fetch('value')
    end

    headers = [
      'language',
      'framework',
      'req/s',
      'latency (p99, ms)',
      'saturation (%)'
    ]

    rows = frameworks.sort_by do |framework|
      reqs = metrics_by_framework.dig(framework.fetch('id'), 'total_requests_per_s')
      [reqs ? 0 : 1, -(reqs || 0), framework.fetch('language'), framework.fetch('label')]
    end.map do |framework|
      metrics = metrics_by_framework[framework.fetch('id')]
      reqs = metrics['total_requests_per_s']
      p99 = metrics['percentile99']
      saturation = metrics['server_cpu_saturation']

      [
        framework.fetch('language'),
        framework.fetch('label'),
        reqs ? format('%.2f', reqs) : '',
        p99 ? format('%.2f', p99 * 1_000) : '',
        saturation ? format('%.2f', saturation * 100) : ''
      ]
    end

    document = Kramdown::Document.new('')
    document.root.children = [kramdown_table(headers, rows)]

    puts MarkdownTableConverter.convert(document.root)
  end
end
