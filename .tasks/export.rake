require 'json'
require 'kramdown'

class MarkdownTableConverter < Kramdown::Converter::Kramdown
  def convert_table(elt, opts)
    opts[:alignment] = el.options[:alignment]
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

  def convert_tr(_elt, opts)
    "| #{el.children.map { |child| convert(child, opts) }.join(' | ')} |\n"
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

    levels = [64, 256, 512]
    frameworks = data.fetch('frameworks').sort_by do |framework|
      [framework.fetch('language'), framework.fetch('label')]
    end

    throughput_by_framework = Hash.new { |hash, key| hash[key] = {} }

    data.fetch('metrics').each do |metric|
      next unless metric.fetch('label') == 'total_requests_per_s'

      throughput_by_framework[metric.fetch('framework_id')][metric.fetch('level')] = metric.fetch('value')
    end

    headers = [
      'language',
      'framework',
      'req/s for 64 concurrency',
      'req/s for 256 concurrency',
      'req/s for 512 concurrency'
    ]

    rows = frameworks.map do |framework|
      row = [
        framework.fetch('language'),
        framework.fetch('label')
      ]

      levels.each do |level|
        value = throughput_by_framework.dig(framework.fetch('id'), level)
        row << (value ? format('%.2f', value) : '')
      end

      row
    end

    document = Kramdown::Document.new('')
    document.root.children = [kramdown_table(headers, rows)]

    puts MarkdownTableConverter.convert(document.root)
  end
end
